with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with CSV;

with AWS.Client, AWS.Response;

procedure Lang_Index(
  HTML_table_categ   : out Unbounded_String; -- languages sorted by category
  HTML_details       : out Unbounded_String; -- details and where you can click the queries
  Text_IO_Monitor    : Boolean:= True;
  -- ^-- only for debug, and with console available
  Dump_when_no_match : Boolean:= True
)
is
  max_lng   : constant:= 200;
  max_eng   : constant:= 20;
  invalid   : constant:= -1;
  hits      : array(1..max_lng, 1..max_eng) of Integer:= (others=> (others => invalid));
  weight    : array(1..max_eng) of Natural;
  name_lng  : array(1..max_lng) of Unbounded_String;
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
  url       : array(1..max_lng, 1..max_eng) of Unbounded_String;
  tot_eng   : Natural:= 0;
  tot_lng   : array(Category) of Natural:= (others => 0);
  sep: constant Character:= ';';
  --
  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  --------------------------------------
  -- Gathering data from the Internet --
  --------------------------------------
  --
  procedure Gathering is
    l, e: File_Type;
    idx_lng : Natural:= 0;
  begin
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
        type Result_type is (ok, no_match, aws_error);
        result: Result_type;
      begin
        idx_lng:= idx_lng + 1;
        tot_lng(any):= Integer'Max(tot_lng(any), idx_lng);
        lng_categ(idx_lng):= Category'Value(cat);
        confidence(idx_lng):= Integer'Value(CSV.Extract(ll, fl, 4, True));
        name_lng(idx_lng):= U(lng);
        Open(e, In_File, "e.csv"); -- Open the engine file
        Skip_Line(e); -- header
        while not End_Of_File(e) loop
          declare
            le : constant String:= Get_Line(e);
            fe : constant CSV.Fields_Bounds:= CSV.Get_Bounds(le, sep);
            eng: constant String:= CSV.Extract(le, fe, 1, True);
            qry: constant String:=
              CSV.Extract(le, fe, 3, True) & "%2B""" & lng_qry & "%20programming""";
            match: constant String := CSV.Extract(le, fe, 4, True);
            skip : constant Natural:= Integer'Value(CSV.Extract(le, fe, 5, True));
            ko   : constant String:= CSV.Extract(le, fe, 6, True);
          begin
            idx_eng:= idx_eng + 1;
            tot_eng:= Integer'Max(tot_eng, idx_eng);
            weight(idx_eng):= Integer'Value(CSV.Extract(le, fe, 2, True));
            name_eng(idx_eng):= U(eng);
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
            begin
              tok_i:= Index(web, match);
              if tok_i = 0 then
                result:= no_match;
                hits(idx_lng, idx_eng):= 0;
              else
                i:= tok_i + match'Length;
                scan:
                for count in 1 .. 1+skip loop
                  r:= 0;
                  while web(i) not in '0'..'9' loop
                    if ko /= "" and then web(i..i+ko'Length-1) = ko then
                      -- e.g. </div> in YouTube when no result
                      exit scan;
                    end if;
                    i:= i + 1;
                  end loop;
                  loop
                    if web(i) in '0'..'9' then
                      r:= r * 10 + Integer'Value(web(i) & "");
                    end if;
                    i:= i + 1;
                    exit when web(i) = '<' or web(i)=' ';
                    -- give up at next tag or space
                  end loop;
                end loop scan;
                hits(idx_lng, idx_eng):= r;
              end if;
              if Dump_when_no_match and result = no_match then
                Create(dump, Out_File, "No_match_" & lng_qry & '_' & eng & ".html");
                Put(dump, web);
                Close(dump);
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
        end loop;
        Close(e);
      end;
    end loop;
    Close(l);
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
    w: array(1..tot_eng) of Float;
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
        rank_eng(l,e):= plausible_hits(l,e) / Float(total_hits(e));
      end loop;
    end loop;
    -- Normalized weights
    for e in 1..tot_eng loop
      sum:= sum + weight(e);
    end loop;
    for e in 1..tot_eng loop
      w(e):= Float(weight(e)) / Float(sum);
    end loop;
    -- Compute average ranking
    for l in 1..tot_lng(any) loop
      rank_avg(any)(l):= (0.0, l);
      for e in 1..tot_eng loop
        rank_avg(any)(l).value:=
          rank_avg(any)(l).value + w(e) * rank_eng(l,e);
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
      Put(str, 100.0 * f,2,0);
      return Trim(str,Left) & '%';
    end Pct;
    --
    htm : Unbounded_String renames HTML_details;
    grd : Unbounded_String renames HTML_table_categ;
  begin
    ------------------------------------
    -- HTML grid with details & links --
    ------------------------------------
    -- Header
    htm:= U(
      "<table border=1>" &
      "<td></td><td>Search engine &rarr;</td>" & ASCII.LF
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
    end loop;
    htm:= htm & "<td></td></tr>" & ASCII.LF;
    htm:= htm & "<tr><td></td><td>&darr; Category</td>";
    for x in 1..2 loop
      for e in 1..tot_eng loop
        htm:= htm & "<td></td>";
      end loop;
    end loop;
    htm:= htm & "<td>&darr; Confidence</td></tr>" & ASCII.LF;
    -- Grid
    for l in 1..tot_lng(any) loop
      htm:= htm & "<tr><td>" & name_lng(l) &
        "</td><td>" &
        To_Lower(Category'Image(lng_categ(l))) &
        "</td>";
      for e in 1..tot_eng loop
        htm:= htm &
          "<td align=center><a target=_blank href=" &
          url(l,e) & ">" &
          Integer'Image(hits(l,e)) &
          "</a></td>";
      end loop;
      for e in 1..tot_eng loop
        htm:= htm &
          "<td align=center>" & Pct(rank_eng(l,e)) & "</td>";
      end loop;
      htm:= htm &
        "<td>" & Integer'Image(confidence(l)) & "%</td>" &
        "</tr>" & ASCII.LF;
    end loop;
    htm:= htm & "</table>" & ASCII.LF;
    ----------------------------------
    -- HTML main tables for display --
    ----------------------------------
    grd:= U(
      "<table border=1 cellspacing=5 cellpadding=5>" &
      "<tr valign=top bgcolor=lightgray>" & ASCII.LF
    );
    for cat in Category loop
      -- Header
      grd:= grd &
        "<td>Language category: <b>" & To_Lower(Category'Image(cat)) &
        "</b><br><br><table border=1 cellspacing=2 cellpadding=2 bgcolor=white>" &
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
    grd:= grd & "</table>" & ASCII.LF;
  end Report;
  --
begin
  Gathering;
  Statistics;
  Report;
end Lang_Index;
