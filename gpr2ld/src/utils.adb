-----------
-- Utils --
-----------

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
                       "Memory_Map",
                        Indexed => True)),
         new String'(Register_New_Attribute
                      ("Size",
                       "Memory_Map",
                       Indexed => True)),
         new String'(Register_New_Attribute
                      ("Address",
                       "Memory_Map",
                       Indexed => True)),
         new String'(Register_New_Attribute
                      ("Memories",
                       "Memory_Map",
                       Is_List => True)),
         new String'(Register_New_Attribute
                      ("Boot_Memory",
                       "Memory_Map")),

         -- CPU --
         new String'(Register_New_Attribute
                      ("Name",
                       "CPU")),
         new String'(Register_New_Attribute
                      ("Float_Handling",
                       "CPU")),
         new String'(Register_New_Attribute
                      ("Number_Of_Interrupts",
                       "CPU")),
         new String'(Register_New_Attribute
                      ("Linker_Template",
                       "CPU")),
         new String'(Register_New_Attribute
                      ("Startup_Template",
                       "CPU")),

         -- Interrupt_Vector --
         new String'(Register_New_Attribute
                      ("Interrupt",
                       "Interrupt_Vector",
                       Indexed => True)),

         -- Architectures_Configuration --
         new String'(Register_New_Attribute
                      ("Dir",
                       "Architectures_Configuration")),
         new String'(Register_New_Attribute
                      ("CPU_Architecture",
                       "Architectures_Configuration",
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

end Utils;
