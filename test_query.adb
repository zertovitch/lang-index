with AWS.Client, AWS.Response;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

procedure Test_query is
  qry: constant String:= "http://www.bing.com/search?q=%2B%22Les%20violons%20longs%22&cc=gb";
  --- qry: constant String:= "http://www.google.com/search?q=%2B%22Les%20violons%20longs%22";
  web: constant String:= AWS.Response.Message_Body(AWS.Client.Get(qry));
  f: File_Type;
begin
  -- Dump
  Create(f, Out_File, "test.html");
  Put(f, web);
  Close(f);
end Test_query;
