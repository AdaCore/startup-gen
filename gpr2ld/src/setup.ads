with GNAT.Strings; use GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

package Setup is

   --  We verify that the arguments are correct and set correctly
   procedure Get_Arguments (Config : in out Command_Line_Configuration;
                           Config_File, Output_Dir : aliased out String_Access)
   with Post => (Config_File.all /= "") or else raise Name_Error
      with "You must pass a file on the command line.";

end Setup;
