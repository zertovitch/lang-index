-- Language Popularity Index
----------------------------

-- The package Lang_Index provides a fully automatic gathering of data
-- using various search engines (see e.csv) about various programming
-- languages (see l.csv). All details are configured and explained
-- in search.xls which contains the sheets for e.csv and l.csv.

-- Legal licensing note:

--  Copyright (c) 2010 Gautier de Montmollin
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

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Lang_Index is

  -------------------------------------------------------
  -- Generate - generate the Language Popularity Index --
  -------------------------------------------------------
  --
  procedure Generate(
    HTML_table_categ   : out Unbounded_String; -- languages sorted by category
    HTML_details       : out Unbounded_String; -- details and where you can click the queries
    Text_IO_Monitor    : Boolean:= True;
    -- ^-- only for debug, and with console available
    Dump_when_no_match : Boolean:= True
  );


  procedure Store_statistics;

  web       : constant String:= "http://lang-index.sf.net/";
  -- hopefully the latest version is at that URL...  ---^

end Lang_Index;