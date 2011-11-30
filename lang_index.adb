-- Language Popularity Index
----------------------------

-- The package Lang_Index provides a fully automatic gathering of data
-- using various search engines (see e.csv) about various programming
-- languages (see l.csv). All details are configured and explained
-- in search.xls which contains the sheets for e.csv and l.csv.
--
-- Simply said, Lang_Index.Generate does the following tasks:
-- 1) gather result counts for each pair {language, engine}
-- 2) apply to them a given confidence factor per language
-- 3) establish one language market share list per search engine
-- 4) establish a market share list, averaged over the search engines,
--     using given weights
-- 5) establish one conditional market share list per language category
--
-- Results of steps 1) to 4) appear from left to right in the detailed
-- HTML table (HTML_details); results of step 5) appear in HTML_table_categ.
--
-- Legal licensing note:
--
--  Copyright (c) 2010..2011 Gautier de Montmollin
--
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with CSV, Time_display;

with AWS.Client, AWS.Response;

package body Lang_Index is

  -- The following are globals, but this package
  -- should obviously used as singleton anyway.

  max_lng    : constant:= 200;
  max_eng    : constant:= 20;
  invalid    : constant:= -1;
  hits       : array(1..max_lng, 1..max_eng) of Integer:= (others=> (others => invalid));
  weight     : array(1..max_eng) of Natural; -- un-normalized weights
  norm_weight: array(1..max_eng) of Float;   -- sum of these weights = 1
  time_filter: array(1..max_eng) of Boolean; -- 1-year filter in query ?
  name_lng  : array(1..max_lng) of Unbounded_String;
  name_lng_qry : array(1..max_lng) of Unbounded_String;
  name_eng  : array(1..max_eng) of Unbounded_String;
  type Category is (any, compiled, script, other);
  lng_categ : array(1..max_lng) of Category;
  confidence: array(1..max_lng) of Natural;
  --
  type Rank_info is record
    value: Float;    -- average normalized hits
    index: Positive; -- index of language (main input table)
  end record;
  type Rank_vector is array(Positive range <>) of Rank_info;
  rank_eng  : array(1..max_lng, 1..max_eng) of Float;
  rank_avg  : array(Category) of Rank_vector(1..max_lng); -- average ranking, sorted
  rank_avg_any_unsorted: Rank_vector(1..max_lng);
  url       : array(1..max_lng, 1..max_eng) of Unbounded_String;
  tot_eng   : Natural:= 0;
  tot_lng   : array(Category) of Natural:= (others => 0);
  sep: constant Character:= ';';
  --
  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;
  --
  now: constant Time:= Clock;
  time_str: constant String:= Time_display(now, False);

  function Replace_Google_Blogs_Special(q: String) return String is
    with_date_last_year: constant String:=
      "as_miny=" & Trim(Integer'Image(Year(now)-1), Left) &
      "&as_minm=" & Trim(Integer'Image(Month(now)), Left) &
      "&as_mind=" & Trim(Integer'Image(Day(now)), Left) & '&';
    special: constant String:= "$google_blogs_special$";
    idx: constant Natural:= Index(q, special);
  begin
    if idx = 0 then
      return q;
    else
      -- replace "$google_blogs_special$" with
      -- "&as_miny=2009&as_minm=4&as_mind=12" (appropriate date, of course)
      return Replace_Slice(
        Source => q,
        Low    => idx,
        High   => idx+special'Length-1,
        By     => with_date_last_year
      );
    end if;
  end;

  procedure Generate(
    HTML_table_categ   : out Unbounded_String; -- languages sorted by category
    HTML_details       : out Unbounded_String; -- details and links where you can click the queries
    Text_IO_Monitor    : Boolean:= True;
    -- ^-- only for debug, and with console available
    Dump_pages : Boolean:= True
  )
  is

    --------------------------------------
    -- Gathering data from the Internet --
    --------------------------------------
    --
    procedure Gathering is
      l, e: File_Type;
      idx_lng : Natural:= 0;
      type Result_type is (ok, no_match, aws_error);
      outcome: array(Result_type) of Natural:= (0,0,0);
    begin
      Create_Path("match");
      Create_Path("no_match");
      Open(l, In_File, "l.csv"); -- Open the language file
      Skip_Line(l); -- header
      while not End_Of_File(l) loop
        declare
          ll     : constant String:= Get_Line(l);
          fl     : constant CSV.Fields_Bounds:= CSV.Get_Bounds(ll, sep);
          lng    : constant String:= CSV.Extract(ll, fl, 1, True);
          lng_qry: constant String:= CSV.Extract(ll, fl, 2, True);
          cat    : constant String:= CSV.Extract(ll, fl, 3, True);
          idx_eng: Natural:= 0;
          result: Result_type;
        begin
          idx_lng:= idx_lng + 1;
          tot_lng(any):= Integer'Max(tot_lng(any), idx_lng);
          lng_categ(idx_lng):= Category'Value(cat);
          confidence(idx_lng):= Integer'Value(CSV.Extract(ll, fl, 4, True));
          name_lng(idx_lng):= U(lng);
          name_lng_qry(idx_lng):= U(lng_qry);
          Open(e, In_File, "e.csv"); -- Open the engine file
          Skip_Line(e); -- header
          while not End_Of_File(e) loop
            declare
              le : constant String:= Get_Line(e);
              fe : constant CSV.Fields_Bounds:= CSV.Get_Bounds(le, sep);
              eng: constant String:= CSV.Extract(le, fe, 1, True);
              qry_0: constant String:=
                CSV.Extract(le, fe, 3, True) & "%2B%22" & lng_qry & "%20programming%22";
              qry: constant String:= Replace_Google_Blogs_Special(qry_0);
              match: constant String := CSV.Extract(le, fe, 4, True);
              skip : constant Natural:= Integer'Value(CSV.Extract(le, fe, 5, True));
              ko_word: constant String:= CSV.Extract(le, fe, 6, True);
              filter : constant Boolean:= Boolean'Value(CSV.Extract(le, fe, 7, True));
            begin
              idx_eng:= idx_eng + 1;
              tot_eng:= Integer'Max(tot_eng, idx_eng);
              weight(idx_eng):= Integer'Value(CSV.Extract(le, fe, 2, True));
              name_eng(idx_eng):= U(eng);
              time_filter(idx_eng):= filter;
              if Text_IO_Monitor then
                Put_Line(lng & " - " & eng & " - " & qry);
                Put_Line("   query: " & qry);
              end if;
              url(idx_lng, idx_eng):= U(qry);
              result:= ok;
              declare
                web: constant String:= AWS.Response.Message_Body(AWS.Client.Get(qry));
                -- Web page is sucked here --^
                dump: File_Type;
                tok_i: Integer;
                i: Positive;
                r: Natural;
                figure: array(1..1+skip) of Natural:= (others => 1234);
                spec_char: Boolean; -- &nbsp; or &#233; with a figure in it!
              begin
                tok_i:= Index(web, match);
                if tok_i = 0 then
                  result:= no_match;
                  r:= 0;
                else -- token found
                  i:= tok_i + match'Length;
                  spec_char:= False;
                  scan:
                  for count in figure'Range loop
                    r:= 0;
                    while (web(i) not in '0'..'9') or spec_char loop
                      if ko_word /= "" and then web(i..i+ko_word'Length-1) = ko_word then
                        if Text_IO_Monitor then
                          Put("Escaping after the following figures: ");
                          for x in 1..count-1 loop
                            Put(Integer'Image(figure(x)) & ',');
                          end loop;
                          New_Line;
                        end if;
                        -- e.g. </div> in YouTube when no result
                        result:= no_match;
                        exit scan;
                      end if;
                      case web(i) is
                        when '&' =>
                          spec_char:= True;
                          if Text_IO_Monitor then
                            Put("[Spec char on]");
                          end if;
                        when ';' =>
                          spec_char:= False;
                          if Text_IO_Monitor then
                            Put("[Spec char off]");
                          end if;
                        when others =>
                          null;
                      end case;
                      i:= i + 1;
                    end loop;
                    collect_digits:
                    loop
                      if web(i) in '0'..'9' then
                        r:= r * 10 + Integer'Value(web(i) & "");
                      end if;
                      i:= i + 1;
                      case web(i) is
                        when '<' | ' ' | '-' | Character'Val(16#A0#) =>
                          -- ^ anything that might separate numbers
                          if web(i) = ' ' and web(i+1) in '0'..'9' then
                            null;
                            -- special case: we have a spacing as thousands separator
                          else
                            exit collect_digits; -- give up at next tag or spacing
                          end if;
                        when others =>
                          null; -- anything else might be part of a number
                                -- (like some thousands separator)
                      end case;
                    end loop collect_digits;
                    figure(count):= r;
                    if Text_IO_Monitor then
                      Put_Line("[figure number" & Integer'Image(count) & " is" & Integer'Image(r) & ']');
                    end if;
                  end loop scan;
                end if;
                hits(idx_lng, idx_eng):= r;
                if Dump_pages then
                  case result is
                    when ok =>
                      Create(dump, Out_File, "match/" & lng_qry & '_' & eng & ".html");
                      Put(dump, web);
                      Close(dump);
                    when no_match =>
                      Create(dump, Out_File, "no_match/" & lng_qry & '_' & eng & ".html");
                      Put(dump, web);
                      Close(dump);
                    when aws_error =>
                      null;
                  end case;
                end if;
              end;
            exception
              when others =>
                result:= aws_error;
            end;
            if Text_IO_Monitor then
              Put_Line(" result : " & Result_type'Image(result));
              Put_Line("   hits :" & Integer'Image(hits(idx_lng, idx_eng)));
              New_Line;
            end if;
            outcome(result):= outcome(result) + 1;
            delay 1.2345;
          end loop;
          Close(e);
        end;
      end loop;
      Close(l);
      if Text_IO_Monitor then
        Put_Line("--------------- Done with queries --------------");
        Put_Line("Engines  :" & Integer'Image(tot_eng));
        Put_Line("Languages:" & Integer'Image(tot_lng(any)));
        for r in outcome'Range loop
          Put_Line(
            "Web pages with outcome... " &
            Result_type'Image(r) & ':' & Integer'Image(outcome(r))
          );
        end loop;
      end if;
    end Gathering;

    ----------------
    -- Statistics --
    ----------------
    --
    procedure Statistics is
      cat: Category;
      function ">"(a,b: Rank_info) return Boolean is
      begin
        return a.value > b.value;
      end ">";
      procedure Sort is
        new Ada.Containers.Generic_Array_Sort(
          Positive,
          Rank_info,
          Rank_vector,
          ">" -- we want a descending sort
        );
      --
      sum: Natural:= 0;
      --
      total_hits: array(1..tot_eng) of Float:= (others => 0.0);
      plausible_hits: array(1..tot_lng(any), 1..tot_eng) of Float;
      total: Float;
    begin
      for e in 1..tot_eng loop
        for l in 1..tot_lng(any) loop
          plausible_hits(l,e):= Float(hits(l,e)) * Float(confidence(l)) * 0.01;
          total_hits(e):= total_hits(e) + plausible_hits(l,e);
        end loop;
        for l in 1..tot_lng(any) loop
          if total_hits(e) > 0.0 then
            rank_eng(l,e):= plausible_hits(l,e) / total_hits(e);
          else
            rank_eng(l,e):= 0.0; -- for this language l, there was no hit at all
          end if;
        end loop;
      end loop;
      -- Normalized weights
      for e in 1..tot_eng loop
        sum:= sum + weight(e);
      end loop;
      for e in 1..tot_eng loop
        norm_weight(e):= Float(weight(e)) / Float(sum);
      end loop;
      -- Compute average ranking
      for l in 1..tot_lng(any) loop
        rank_avg(any)(l):= (0.0, l);
        for e in 1..tot_eng loop
          rank_avg(any)(l).value:=
            rank_avg(any)(l).value + norm_weight(e) * rank_eng(l,e);
        end loop;
        -- Make category-sensitive ranking
        cat:= lng_categ(l);
        tot_lng(cat):= tot_lng(cat) + 1;
        rank_avg(cat)(tot_lng(cat)):= (rank_avg(any)(l).value, l);
      end loop;
      -- We need to reweight the hit shares for partial categories
      for c in Category'Succ(Category'First) .. Category'Last loop
        total:= 0.0;
        for lc in 1..tot_lng(c) loop
          total:= total + rank_avg(c)(lc).value;
        end loop;
        if total > 0.0 then
          for lc in 1..tot_lng(c) loop
            rank_avg(c)(lc).value:= rank_avg(c)(lc).value / total;
          end loop;
        end if;
      end loop;
      -- Keep the unsorted ranking, for the details
      rank_avg_any_unsorted:= rank_avg(any);
      -- Sort the rankings per category
      for c in Category loop
        Sort(rank_avg(c)(1..tot_lng(c)));
      end loop;
    end Statistics;

    -------------------------------
    -- Produce nice HTML reports --
    -------------------------------
    --
    procedure Report is
      --
      function Pct(f: Float) return String is
        str: String:= "          ";
      begin
        Put(str, 100.0 * f, 3,0);
        return Trim(str,Left) & '%';
      end Pct;
      --
      function Sep1000(i: Natural) return String is
        nbsp: constant String:= "&nbsp;";
        str: String(1..10+3*nbsp'Length); -- "1_000_000_000"
        l: Natural:= str'Last+1; -- we go from right to left - arabic numbers!
        c: Natural:= 0;
        j: Integer:= i;
      begin
        if i = 0 then
          return "0";
        end if;
        loop
          l:= l - 1;
          str(l):= Character'Val(j mod 10 + Character'Pos('0'));
          c:= c + 1;
          j:= j / 10;
          exit when j = 0;
          if c mod 3 = 0 then
            for k in reverse nbsp'Range loop
              l:= l - 1;
              str(l):= nbsp(k);
            end loop;
          end if;
        end loop;
        return str(l..str'Last);
      end Sep1000;
      --
      function Encode_URL(u0: Unbounded_String) return Unbounded_String is
      -- make http://validator.w3.org happier
        s: String:= To_String(u0);
        u: Unbounded_String;
      begin
        for i in s'Range loop
          case s(i) is
            when '&' =>
              u:= u & "&amp;";
            when others =>
              u:= u & s(i);
          end case;
        end loop;
        return u;
      end Encode_URL;
      --
      htm : Unbounded_String renames HTML_details;
      grd : Unbounded_String renames HTML_table_categ;
      html_header: constant String:=
        "<a href=""" & web & """>Language Popularity Index</a>" &
        " - Web queries done on: " & time_str & "<br><br>" & ASCII.LF;
      --
      function Category_Image(c: Category) return String is
      begin
        case c is
          when compiled =>
            return "general-purpose";
          when others   =>
            return Category'Image(c);
        end case;
      end Category_Image;
    begin
      ------------------------------------
      -- HTML grid with details & links --
      ------------------------------------
      -- Header
      htm:= U(
        html_header &
        "<font face=""Calibri, Tahoma, Arial""><table border=1>" &
        "<td></td><td></td><td><b>Search engine &rarr;</b></td>" & ASCII.LF
      );
      for x in 1..2 loop
        for e in 1..tot_eng loop
          htm:= htm & "<td ";
          if x = 1 then
            htm:= htm & "bgcolor=lightblue";
          else
            htm:= htm & "bgcolor=yellow";
          end if;
          htm:= htm & ">" & name_eng(e) & "</td>";
        end loop;
        if x = 1 then
          htm:= htm & "<td></td>";
        end if;
      end loop;
      htm:= htm &
        "<td>Average engine</td></tr>" & ASCII.LF &
        "<tr><td></td><td></td><td></td>";
      for e in 1..tot_eng loop
        if time_filter(e) then
          htm:= htm & "<td>1-year filter</td>";
        else
          htm:= htm & "<td>no filter</td>";
        end if;
      end loop;
      htm:= htm & "<td bgcolor=lightgreen><b>Weight&nbsp;&rarr;<br>Normalized&nbsp;&rarr;</b></td>";
      for e in 1..tot_eng loop
        htm:= htm &
          "<td bgcolor=lightgreen align=center>" &
          Integer'Image(weight(e)) &
          "<br>" &
          Pct(norm_weight(e)) &
          "</td>";
      end loop;
      htm:= htm & "<td></td></tr>" & ASCII.LF;
      htm:= htm &
        "<tr><td>Language display name</td><td>Name in query</td>" &
        "<td><b>Category's short name &darr;</b></td>";
      for e in 1..tot_eng loop
        htm:= htm & "<td align=center>Results</td>";
      end loop;
      htm:= htm & "<td bgcolor=#D3D3D3><b>Confidence &darr;</b></td>";
      for e in 1..tot_eng+1 loop
        htm:= htm & "<td align=center>Share</td>";
      end loop;
      htm:= htm & "</tr>" & ASCII.LF;
      -- Grid
      for l in 1..tot_lng(any) loop
        htm:= htm &
          "<tr><td>" & name_lng(l) & "</td><td>" & name_lng_qry(l) & "</td><td>" &
          To_Lower(Category_Image(lng_categ(l))) &
          "</td>";
        for e in 1..tot_eng loop
          htm:= htm &
            "<td align=center><a target=_blank href=""" &
            Encode_URL(url(l,e)) & """>" &
            Sep1000(hits(l,e)) &
            "</a></td>";
        end loop;
        htm:= htm &
          "<td bgcolor=#D3D3D3>" & Integer'Image(confidence(l)) & "%</td>";
        for e in 1..tot_eng loop
          htm:= htm &
            "<td align=center>" & Pct(rank_eng(l,e)) & "</td>";
        end loop;
        htm:= htm &
          "<td align=center>" & Pct(rank_avg_any_unsorted(l).value) & "</td>";
        htm:= htm & ASCII.LF;
      end loop;
      htm:= htm & "</tr></table></font>" & ASCII.LF;
      ----------------------------------
      -- HTML main tables for display --
      ----------------------------------
      grd:= U(
        html_header &
        "<font face=""Calibri, Tahoma, Arial""><table border=1 cellspacing=5 cellpadding=5>" &
        "<tr valign=top bgcolor=#D3D3D3>" & ASCII.LF
      );
      for cat in Category loop
        -- Header
        grd:= grd &
          "<td>Language category:<b><br>" & To_Lower(Category_Image(cat)) &
          "&nbsp; <a href=#categ>*)</a></b><br>" &
          "<br><table border=1 cellspacing=2 cellpadding=2 bgcolor=white>" &
          "<tr><td>Rank</td><td>Name</td><td>Share</td>";
        -- Grid
        for lc in 1..tot_lng(cat) loop
          grd:= grd &
            "<tr><td>" & Integer'Image(lc) &
            "</td><td>" & name_lng(rank_avg(cat)(lc).index) &
            "</td><td>" & Pct(rank_avg(cat)(lc).value) &
            "</td></tr>" & ASCII.LF;
        end loop;
        grd:= grd & "</table></td>" & ASCII.LF;
      end loop;
      grd:= grd & "</tr></table></font>" & ASCII.LF;
    end Report;
    --
  begin
    Gathering;
    Statistics;
    Report;
  end Generate;

  procedure Export_statistics is
  begin
    -- Details.
    -- Some informations here are pretty redundant,
    -- like the confidence, weights.
    for l in 1..tot_lng(any) loop
      for e in 1..tot_eng loop
        Export_detail(
          S(name_lng(l)),
          S(name_eng(e)),
          To_Lower(Category'Image(lng_categ(l))),
          hits(l,e),
          Float(confidence(l)) / 100.0,
          norm_weight(e),
          now
        );
      end loop;
    end loop;
    -- "Hit parade"'s
    for cat in Category loop
      for lc in 1..tot_lng(cat) loop
        Export_share(
          S(name_lng(rank_avg(cat)(lc).index)),
          To_Lower(Category'Image(cat)),
          rank_avg(cat)(lc).value,
          now
        );
      end loop;
    end loop;
  end Export_statistics;

end Lang_Index;
