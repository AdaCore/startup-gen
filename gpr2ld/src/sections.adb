--------------
-- Sections --
--------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Sections is

   ------------------
   -- Make_Section --
   ------------------

   function Make_Section
      (Boot_Memory  : Unbounded_String;
       Name         : Unbounded_String;
       Reloc_Memory : Unbounded_String;
       Force_Init   : Boolean := False)
       return Section
   is
      To_Init : constant Boolean := Force_Init or else
                                       (Boot_Memory /= Reloc_Memory);
      Lowered_Name : constant Unbounded_String :=
         To_Unbounded_String (To_Lower (To_String (Name)));
      Temp : constant Section :=
         (Name               => Lowered_Name,
          Reloc_Memory       => Reloc_Memory,
          To_Init            => To_Init,
          Additional_Content => Unbounded_String_Vect.Empty_Vector);
   begin
     return Temp;
   end Make_Section;

end Sections;
