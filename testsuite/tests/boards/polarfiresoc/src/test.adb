with Ada.Text_IO;    use Ada.Text_IO;
with Interfaces; use Interfaces;

package body Test is

   RAM_Data : array (1 .. 10) of Unsigned_8 := (others => 42);

   RAM_BSS : array (1 .. 10) of Unsigned_8 := (others => 0);

   Heap_Start : Unsigned_32;
   pragma Import (C, Heap_Start, "__heap_start");

   Heap_End : Unsigned_32;
   pragma Import (C, Heap_End, "__heap_end");

   --------------------
   -- Check_Memories --
   --------------------

   procedure Check_Memories is
      Result : Boolean := True;
   begin
      Put_Line ("Boot memory: RAM");

      if not In_RAM (RAM_Data'Address) then
         Put_Line ("Wrong address for RAM_Data");
         Result := False;
      end if;

      if not In_RAM (RAM_BSS'Address) then
         Put_Line ("Wrong address for Heap_End");
         Result := False;
      end if;

      if not In_RAM (Heap_Start'Address) then
         Put_Line ("Wrong address for Heap_Start");
         Result := False;
      end if;

      if not In_RAM (Heap_End'Address) then
         Put_Line ("Wrong address for Heap_End");
         Result := False;
      end if;

      --  Check that the bss was cleared
      for Elt of RAM_BSS loop
         if Elt /= 0 then
            Put_Line ("Wrong value for RAM_BSS");
            Result := False;
         end if;
      end loop;

      if Result then
         Put_Line ("PASS: Memories checks");
      else
         Put_Line ("FAIL: Memories checks");
      end if;
   end Check_Memories;

end Test;
