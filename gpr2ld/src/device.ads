pragma Ada_12;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

with File_Writer; use File_Writer;

------------
-- Device --
------------

package Device is

   type Spec is tagged private;

   package Package_String_Vector is new Ada.Containers.Vectors (Positive,
      Unbounded_String);

   package SV renames Package_String_Vector;
   procedure Get_Memory_List_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Get_CPU_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Display (Self : in out Spec);

   procedure Dump_Linker_Script (Self : in out Spec; VF : Virtual_File);

   procedure Dump_Memory_Map (Self : in out Spec; VF : Virtual_File);

private

   type Interrupt_Type is record
      Name : Unbounded_String;
   end record;

   type Float_Type is (Hard, Soft);

   type CPU_Type is record
      Name           : Unbounded_String;
      Float_Handling : Float_Type;
   end record;

   type Memory_Kind is (RAM, ROM, TCM, CCM);

   type Memory_Type is record
      Name  : Unbounded_String;
      Start : Unbounded_String;
      Size  : Unbounded_String;
      Kind  : Memory_Kind;
   end record;

   package Int_Vect is new Ada.Containers.Vectors (Positive, Interrupt_Type);
   package Mem_Vect is new Ada.Containers.Vectors (Positive, Memory_Type);

   type Spec is tagged record
      Memory          : Mem_Vect.Vector;
      CPU             : CPU_Type;
      InterruptVector : Int_Vect.Vector;
   end record;

   --  Private procedures  --

   procedure Dump_Sections
      (Self : in out Spec;
       File : in out Indented_File_Writer);

   procedure Dump_Memory
      (File        : in out Indented_File_Writer;
       Name        :  Unbounded_String;
       Permissions :  Unbounded_String;
       Start       :  Unbounded_String;
       Size        : Unbounded_String);

end Device;
