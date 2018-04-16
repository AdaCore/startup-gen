with GNAT.Strings; use GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;

package Setup is

   --  We verify that the arguments are correct and set correctly
   procedure Get_Arguments (Config : in out Command_Line_Configuration;
                           Config_File, Output_Dir : aliased out String_Access)
   with Post => (Config_File /= null and then Output_Dir /= null);
private

   procedure Callback (Switch, Param, Section : String);

end Setup;
