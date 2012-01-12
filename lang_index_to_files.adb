-- Language Popularity Index - Standalone application
-----------------------------------------------------
--
-- Gather all data for the index and dump it as HTML chunks to be inserted.

-- Legal licensing note:

--  Copyright (c) 2010..2012 Gautier de Montmollin
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


with Lang_Index;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

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
  Lang_Index.Generate(
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
