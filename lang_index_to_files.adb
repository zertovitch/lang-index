with Lang_Index;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Lang_Index_to_Files is
  HTML_table_categ: Unbounded_String; -- table with languages sorted by category
  HTML_details    : Unbounded_String; -- table with details and where you can click the queries
  f: File_Type;
  --
  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;
  --
begin
  Lang_Index(
    HTML_table_categ,
    HTML_details
  );
  -- Dump
  Create(f, Out_File, "grids.html");
  Put(f, S(HTML_table_categ));
  Close(f);
  --
  Create(f, Out_File, "details.html");
  Put(f, S(HTML_details));
  Close(f);
end Lang_Index_to_Files;