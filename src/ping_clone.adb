with Ada.Command_Line;
with Ada.Text_IO;

procedure Ping_Clone is
begin
   for Index in 1 .. Ada.Command_Line.Argument_Count loop
      Ada.Text_IO.Put_Line(Ada.Command_Line.Argument(Index));
   end loop;
end Ping_Clone;
