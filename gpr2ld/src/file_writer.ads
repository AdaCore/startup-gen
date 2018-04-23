with GNATCOLL.VFS;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package File_Writer is

   type Indented_File_Writer is tagged private;

   function Make (Handle : GNATCOLL.VFS.Writable_File;
                  Indentation_Size : Positive := 2;
                  Indentation_Character : Character := ' ')
                  return Indented_File_Writer;

   procedure Put_Line (Self : in out Indented_File_Writer;
                    Line : String;
                    Indented : Boolean := False);

   procedure Put_Line (Self : in out Indented_File_Writer;
                    Line : Unbounded_String;
                    Indented : Boolean := False);

   procedure Put_Indented_Line (Self : in out Indented_File_Writer;
                                Line : String;
                                Indented : Boolean := True) renames Put_Line;

   procedure Put_Indented_Line (Self : in out Indented_File_Writer;
                                Line : Unbounded_String;
                                Indented : Boolean := True) renames Put_Line;

   procedure Indent (Self : in out Indented_File_Writer);

   procedure Unindent (Self : in out Indented_File_Writer);

   procedure Close (Self : in out Indented_File_Writer);

private

   type Indented_File_Writer is tagged
      record
         Handle : GNATCOLL.VFS.Writable_File;
         Indentation_Level : Natural;
         Indentation_Size : Positive;
         Indentation_Character : Character;
      end record;

end File_Writer;
