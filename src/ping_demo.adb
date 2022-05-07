with Ada.Command_Line;
with Ada.Text_IO;

with Networking.ICMP;

procedure Ping_Demo is
begin
   for Index in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Host : constant String := Ada.Command_Line.Argument(Index);
      begin
         Ada.Text_IO.Put_Line(Host);
         Networking.ICMP.Ping(Host, "hello world");
      end;
   end loop;
end Ping_Demo;
