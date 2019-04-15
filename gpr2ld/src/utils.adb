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
                      ("Name",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Float_Handling",
                       Prj_Package_Name)),
         new String'(Register_New_Attribute
                      ("Number_Of_Interrupts",
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
