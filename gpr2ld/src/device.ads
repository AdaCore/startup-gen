pragma Ada_12;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.Projects; use GNATCOLL.Projects;

package Device is

   type Spec is tagged private;

   procedure Set_Memory_List (Self : in out Spec; Path : String);

   procedure Dump (Self : Spec);

private
   type Interrupt_Type is
      record
         Name : Unbounded_String;
      end record;

   type Float_Type is (Hard, Soft);

   type CPU_Type is
      record
         Name : Unbounded_String;
         Float_Handling : Float_Type;
      end record;

   type Memory_Kind is (RAM, ROM, TCM, CCM);

   --  TODO: replace by Natural and Positive typesj
   type Memory_Type is
      record
         Name : Unbounded_String;
         Start : Unbounded_String;
         Size : Unbounded_String;
         Kind : Memory_Kind;
      end record;

   package Int_Vect is new Ada.Containers.Vectors (Positive, Interrupt_Type);
   package Mem_Vect is new Ada.Containers.Vectors (Positive, Memory_Type);

   type Spec is tagged
      record
         Memory : Mem_Vect.Vector;
         CPU : CPU_Type;
         InterruptVector : Int_Vect.Vector;
      end record;

end Device;
