with Ada.Text_IO; use Ada.Text_IO;

package body Setup is

   procedure Get_Arguments (Config : in out Command_Line_Configuration;
                        Config_File, Output_Dir : aliased out String_Access) is
   begin

      Define_Switch (Config, Output_Dir'Access, "-o:",
         Help => "Directory in which generated files will be put.");

      Getopt (Config);

      Config_File := new String'(Get_Argument);

      Put_Line ("Config " & Config_File.all);
      Put_Line ("Out Dir " & Output_Dir.all);

   end Get_Arguments;

end Setup;
