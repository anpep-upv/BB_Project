---------------------------------------------
--            A D C _ T E S T S            --
--                                         --
--  COMPLETAR CONFORME A EJERCICIOS 1 Y 2  --
--                                         --
---------------------------------------------
with BB, BB.ADC;                   use BB, BB.ADC;
with CSV_Logs;                     use CSV_Logs;
with Ada.Text_IO;                  use Ada.Text_IO;

procedure ADC_Test is

   -----------------------
   --  ADC_To_Position  --
   -----------------------

   function ADC_To_Position (DR_Value : ADC_Register) return Position is

     -- Completar...

   --------------------
   --  Polling_Test  --
   --------------------

   procedure Polling_Test is

      Conversion : ADC_Register;  --  To store ADU_Count
      Pos_mm     : Position;      --  Position corresponding to Conversion
      Latency    : Duration;      --  Latency of conversion

   begin

      Open_Log_Session (File_Name => "polling_test.csv");
      Log_Text ("ADC_Conv, Pos_mm, Latency");


      for I in 1 .. 100 loop

         --  Completar

         Log_Data ((Float (Conversion), Pos_mm, Float (Latency)));

      end loop;

      Close_Log_Session;

   end Polling_Test;

   ----------------------
   --  Interrupt_Test  --
   ----------------------

   procedure Interrupt_Test is

      Conversion : ADC_Register;  --  To store ADU_Count
      Pos_mm     : Position;      --  Position corresponding to Conversion
      Latency    : Duration;      --  Latency of conversion

   begin
      Open_Log_Session ("interrupt_test.csv");
      Log_Text ("ADC_Conv, Pos_mm, Latency");

      --  Completar

      for I in 1 .. 100 loop

         -- Completar

         Log_Data ((Float (Conversion), Pos_mm, Float (Latency)));

      end loop;

      Close_Log_Session;

   end Interrupt_Test;

   --------------------
   --  Main Program  --
   --------------------

begin

   Put_Line ("POLLING Test");
   Polling_Test;

   --Put_Line ("INTERRUPT Test");
   --Interrupt_Test;

   Put_Line ("End of ADC Test");

end ADC_Test;
