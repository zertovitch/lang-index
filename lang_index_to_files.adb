with Lang_Index;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Lang_Index_to_Files is
  HTML_table  : Unbounded_String;
  CSV_details : Unbounded_String;
  f: File_Type;
  --
  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;
  --
begin
  Lang_Index(HTML_table, CSV_details);
  Create(f, Out_File, "details.csv");
  Put(f, S(CSV_details));
  Close(f);
end Lang_Index_to_Files;