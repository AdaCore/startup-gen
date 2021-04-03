------------------------------------------------------------------------------
--                                                                          --
--                               startup-gen                                --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Command_Line;
with GNAT.Strings;

with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with Setup;
with Utils;
with Device;

procedure Main is

   use GNAT.Strings;
   use Setup;

   Input : aliased Command_Line_Values;

begin
   Input.Get_Arguments;

   Utils.Register_Memory_Map_Attributes;

   declare
      Env          : Project_Environment_Access;
      Tree         : Project_Tree;

      Project_File : constant Virtual_File :=
        Create_From_Base (Filesystem_String (Input.Project_File.all));

      --  Spec representing the target device
      Spec : Device.Spec;

      Spec_Valid : Boolean := False;

   begin
      Initialize (Env);

      Apply_Scenario_Variables (Env);

      Tree.Load
        (Root_Project_Path => Project_File,
         Env               => Env,
         Packages_To_Check => All_Packs);

      -- Prepare the spec using the root project
      Spec.Prepare_From_Project
        (Spec_Project => Tree.Root_Project,
         Spec_Valid   => Spec_Valid);

      if Spec_Valid then

         if Input.Print_Tags then
            Spec.Dump_Translate_Table;
         end if;

         if Input.Linker_File /= null
           and then
            Input.Linker_File.all /= ""
         then
            Spec.Dump_Linker_Script (Input.Linker_File.all);
         end if;

         if Input.Startup_Code_File /= null
           and then
            Input.Startup_Code_File.all /= ""
         then
            Spec.Dump_Startup_Code (Input.Startup_Code_File.all);
         end if;

         Spec.Display;

      end if;
   end;
exception
   --  We catch the exception from the command line when
   --  the user called the executable with "-h" or "--help"
   when GNAT.Command_Line.Exit_From_Command_Line =>
      New_Line;

   when GNATCOLL.Projects.Invalid_Project =>
      Utils.Error
        ("Project file """ & Input.Project_File.all & """ not found.");
   when Utils.Exit_Exc => null;

end Main;
