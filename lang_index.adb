with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with CSV;

with AWS.Client, AWS.Response;

procedure Lang_Index(
  HTML_table     : out Unbounded_String;
  CSV_details    : out Unbounded_String;
  Text_IO_Monitor: Boolean:= True
  -- ^-- only for debug, and with console available
)
is
  max_lng   : constant:= 200;
  max_eng   : constant:= 20;
  invalid   : constant:= -1;
  hits      : array(1..max_lng, 1..max_eng) of Integer:= (others=> (others => invalid));
  total_hits: array(1..max_eng) of Integer:= (others => invalid);
  weight    : array(1..max_eng) of Natural;
  name_lng  : array(1..max_lng) of Unbounded_String;
  name_eng  : array(1..max_eng) of Unbounded_String;
  category  : array(1..max_lng) of Character;
  confidence: array(1..max_lng) of Natural;
  rank_eng  : array(1..max_lng, 1..max_eng) of Float;
  tot_lng   : Natural:= 0;
  tot_eng   : Natural:= 0;
  sep: constant Character:= ';';
  --
  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;
  --
  procedure Gathering is
    l, e: File_Type;
    idx_lng,
    idx_eng : Natural;
    type Result_type is (ok, no_match, aws_error);
    result: Result_type;
  begin
    Open(l, In_File, "l.csv"); -- Open the language file
    Skip_Line(l); -- header
    idx_lng:= 0;
    while not End_Of_File(l) loop
      declare
        ll : constant String:= Get_Line(l);
        fl : constant CSV.Fields_Bounds:= CSV.Get_Bounds(ll, sep);
        lng: constant String:= CSV.Extract(ll, fl, 1, True);
        lng_qry: constant String:= CSV.Extract(ll, fl, 2, True);
        cat: constant String:= CSV.Extract(ll, fl, 3, True);
      begin
        idx_lng:= idx_lng + 1;
        tot_lng:= Integer'Max(tot_lng, idx_lng);
        category(idx_lng):= cat(cat'First);
        confidence(idx_lng):= Integer'Value(CSV.Extract(ll, fl, 4, True));
        name_lng(idx_lng):= U(lng);
        Open(e, In_File, "e.csv"); -- Open the engine file
        Skip_Line(e); -- header
        idx_eng:= 0;
        while not End_Of_File(e) loop
          declare
            le : constant String:= Get_Line(e);
            fe : constant CSV.Fields_Bounds:= CSV.Get_Bounds(le, sep);
            eng: constant String:= CSV.Extract(le, fe, 1, True);
            qry: constant String:= CSV.Extract(le, fe, 3, True) & "%2B""" & lng_qry & "%20programming""";
            match: constant String:= CSV.Extract(le, fe, 4, True);
            tok_i: Integer;
            i: Positive;
            r: Natural:= 0;
          begin
            idx_eng:= idx_eng + 1;
            tot_eng:= Integer'Max(tot_eng, idx_eng);
            weight(idx_eng):= Integer'Value(CSV.Extract(le, fe, 2, True));
            name_eng(idx_eng):= U(eng);
            if Text_IO_Monitor then
              Put_Line(lng & " - " & eng & " - " & qry);
              Put_Line("   query: " & qry);
            end if;
            result:= ok;
            declare
              web: constant String:= AWS.Response.Message_Body(AWS.Client.Get(qry));
              -- Web page is sucked here --^
            begin
              tok_i:= Index(web, match);
              if tok_i = 0 then
                result:= no_match;
              else
                i:= tok_i + match'Length;
                while web(i) not in '0'..'9' loop
                  i:= i + 1;
                end loop;
                loop
                  if web(i) in '0'..'9' then
                    r:= r * 10 + Integer'Value(web(i) & "");
                  end if;
                  i:= i + 1;
                  exit when web(i) = '<'; -- give up at next tag
                end loop;
                hits(idx_lng, idx_eng):= r;
              end if;
              if Text_IO_Monitor and result = no_match then
                Put(web);
                New_Line;
--                Put("Enter");
--                Skip_Line;
              end if;
            end;
          exception
            when others =>
              result:= aws_error;
          end;
          if Text_IO_Monitor then
            Put_Line(" result :" & Result_type'Image(result));
            Put_Line("   hits :" & Integer'Image(hits(idx_lng, idx_eng)));
            New_Line;
          end if;
        end loop;
        Close(e);
      end;
    end loop;
    Close(l);
  end Gathering;
  --
  procedure Statistics is
  begin
    for e in 1..tot_eng loop
      total_hits(e):= 0;
      for l in 1..tot_lng loop
        total_hits(e):= total_hits(e) + hits(l,e);
      end loop;
      for l in 1..tot_lng loop
        rank_eng(l,e):= Float(hits(l,e)) / Float(total_hits(e));
      end loop;
    end loop;
  end Statistics;
  --
  procedure Report is
  begin
    -- Header
    CSV_details:= U("" & sep);
    for x in 1..2 loop
      for e in 1..tot_eng loop
        CSV_details:= CSV_details & name_eng(e) & sep;
      end loop;
      CSV_details:= CSV_details & sep;
    end loop;
    CSV_details:= CSV_details & ASCII.LF;
    -- Grid
    for l in 1..tot_lng loop
      CSV_details:= CSV_details & name_lng(l) & sep;
      for e in 1..tot_eng loop
        CSV_details:= CSV_details & U(Integer'Image(hits(l,e)) & sep);
      end loop;
      CSV_details:= CSV_details & sep;
      for e in 1..tot_eng loop
        CSV_details:= CSV_details & U(Float'Image(rank_eng(l,e)) & sep);
      end loop;
      CSV_details:= CSV_details & ASCII.LF;
    end loop;
  end Report;
  --
begin
  Gathering;
  Statistics;
  Report;
end Lang_Index; 