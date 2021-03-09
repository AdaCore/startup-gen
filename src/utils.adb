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

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package body Utils is

   ------------------------------------
   -- Register_Memory_Map_Attributes --
   ------------------------------------

   procedure Register_Memory_Map_Attributes is

      type Err_Str_Access is access constant String;
      type Strings is array (Integer range <>) of Err_Str_Access;

      --  Register attributes and save the potential errors in an array
      Errors : constant Strings :=
        (

         -- Memory_Map --
         new String'(Register_New_Attribute
                      ("Mem_Kind",
                       Prj_Package_Name,
                        Indexed => True)),
         new String'(Register_New_Attribute
                      ("Size",
                       Prj_Package_Name,
                       Indexed => True)),
         new String'(Register_New_Attribute
                      ("Address",
                       Prj_Package_Name,
                       Indexed => True)),
         new String'(Register_New_Attribute
                      ("Memories",
                       Prj_Package_Name,
                       Is_List => True)),
         new String'(Register_New_Attribute
                      ("Boot_Memory",
                       Prj_Package_Name)),

         -- CPU --
         new String'(Register_New_Attribute
                      ("CPU_Name",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Float_Handling",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Number_Of_Interrupts",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Main_Stack_Size",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Main_Stack_Memory",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Linker_Template",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Startup_Template",
                       Prj_Package_Name)),

         -- Interrupt_Vector --
         new String'(Register_New_Attribute
                      ("Interrupt",
                       Prj_Package_Name,
                       Indexed => True)),

         -- User_Tag --
         new String'(Register_New_Attribute
                      ("User_Tag",
                       Prj_Package_Name,
                       Indexed => True))
        );
   begin
      for Str of Errors loop
         if Str.all /= "" then
            raise Program_Error with "Could not register new attribute " &
               Str.all;
         end if;
      end loop;
   end Register_Memory_Map_Attributes;

   -------------
   -- Warning --
   -------------

   procedure Warning (Msg : String) is
   begin
      Put_Line (Standard_Error, "Warning: " & Msg);
   end Warning;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Error, "Error: " & Msg);
      Set_Exit_Status (Failure);
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error (Msg : String) is
   begin
      Error (Msg);
      raise Exit_Exc;
   end Fatal_Error;

end Utils;
