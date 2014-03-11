-- This snippet is from Wasabee (Wasabee.Net)

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with GNAT.Sockets ;                     use GNAT.Sockets ;

package Get_HTTP is

   Agent: constant String:= 
     "Mozilla/5.0 (Windows NT 6.1) " & 
     "AppleWebKit/537.36 (KHTML, like Gecko) " &
     "Chrome/33.0.1750.146 Safari/537.36";

   procedure Get_HTTP_Content (
     Host     : in String ;
     Resource : in String ;
     Port     : in Port_Type ;
	 Content  : in out Unbounded_String
   );

   function Get_HTTP_Content (
     Host     : in String ;
     Resource : in String ;
     Port     : in Port_Type
   )
   return String;
   
end Get_HTTP;
