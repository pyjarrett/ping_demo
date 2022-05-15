with Ada.Command_Line;

with Networking.ICMP;

procedure Ping_Demo is
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Networking.ICMP.Ping (Ada.Command_Line.Argument (1), "");
   elsif Ada.Command_Line.Argument_Count = 2 then
      Networking.ICMP.Ping (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2));
   end if;
end Ping_Demo;
