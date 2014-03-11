with Ada.Streams;
-- with Ada.Text_IO;                       use Ada.Text_IO;

package body Get_HTTP is

   -- trace: Boolean:= True;

   procedure Get_HTTP_Content (
     Host     : in String ;
     Resource : in String ;
     Port     : in Port_Type ;
	 Content  : in out Unbounded_String
   ) 
   is
      Client       : Socket_Type ;
      Address      : Sock_Addr_Type;
      Channel      : Stream_Access ;
      Send         : constant String :=  (1 => ASCII.CR, 2 => ASCII.LF);
      Offset       : Ada.Streams.Stream_Element_Count ;
      Data         : Ada.Streams.Stream_Element_Array (1 .. 1024) ;
      request      : constant String:= 
                       "GET " & Resource & " HTTP/1.1" & 
                       Send & 
                       "Host: " & Host & Send & 
                       "User-Agent: " & Agent &
                       Send & Send;
      use Ada.Streams;
   begin
      Create_Socket(Client);
      Address.Addr := Addresses (Get_Host_By_Name(Host));
      Address.Port := Port;
      Connect_Socket (Client, Address);
      Channel := Stream(Client);
      String'Write (Channel, request);      
      loop
         Ada.Streams.Read (Channel.all, Data, Offset) ;
         exit when Offset = 0;
         for I in 1 .. Offset loop
            Append(Content, Character'Val(Data(I)));
         end loop;
      end loop;    
      Close_Socket(Client);
   end Get_HTTP_Content;

   function Get_HTTP_Content (
     Host     : in String ;
     Resource : in String ;
     Port     : in Port_Type
   )
   return String is
     Content  : Unbounded_String;
   begin
     Get_HTTP_Content(Host, Resource, Port, content);
     return To_String(content);
   end Get_HTTP_Content;
   
end Get_HTTP;
