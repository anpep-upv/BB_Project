---------------------------------------------
--            A D C _ T E S T S            --
--                                         --
--  COMPLETAR CONFORME A EJERCICIOS 1 Y 2  --
--                                         --
---------------------------------------------
with BB, BB.ADC;
use BB, BB.ADC;
with CSV_Logs;          use CSV_Logs;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Real_Time;     use Ada.Real_Time;

procedure ADC_Test is

   -----------------------
   --  ADC_To_Position  --
   -----------------------

   function ADC_To_Position (DR_Value : ADC_Register) return Float is
      -- Obtain first 12 bits
      ADC_Count : ADC_Register := DR_Value and (2**12 - 1);
   begin
      return Min_Position +
        Float (ADC_Count) * ((Max_Position - Min_Position) / 4095.0);
   end ADC_To_Position;

   --------------
   --  Get_CR  --
   --------------

   function Get_CR
     (Trigger : Boolean := False; Interrupt_Enable : Boolean := False)
      return ADC_Register
   is
      Flag_TRG : ADC_Register := 2**0;  -- Bit 0
      Flag_IE  : ADC_Register := 2**2;  -- Bit 2

      Result : ADC_Register := 0;
   begin
      if Trigger then
         Result := Result or Flag_TRG;
      end if;

      if Interrupt_Enable then
         Result := Result or Flag_IE;
      end if;

      return Result;
   end Get_CR;

   ------------------------------
   --  Is_Conversion_Complete  --
   ------------------------------

   function Is_Conversion_Complete
     (Data_Register : ADC_Register) return Boolean
   is
      Flag_EOC : ADC_Register := 2**15; -- Bit 15
   begin
      return (Data_Register and Flag_EOC) = Flag_EOC;
   end Is_Conversion_Complete;

   --------------------
   --  Polling_Test  --
   --------------------

   procedure Polling_Test is

      Conversion : ADC_Register := 0;  --  To store ADU_Count
      Pos_mm     : Position;           --  Position corresponding to Conversion
      Latency    : Duration;           --  Latency of conversion

      Clock_Initial : Time;            --  Used to measure latency of conversion
   begin

      Open_Log_Session (File_Name => "polling_test.csv");
      Log_Text ("ADC_Conv, Pos_mm, Latency");

      for I in 1 .. 100 loop

         -- Trigger conversion with interrupts disabled
         Write_CR (Get_CR (Trigger => True));

         -- Wait until the conversion is done by polling DR
         Clock_Initial := Clock;

         while not Is_Conversion_Complete (Conversion) loop
            Conversion := Read_DR;
         end loop;

         -- Calculate ADC latency
         Latency := To_Duration (Clock - Clock_Initial);

         -- Obtain first 12 MSBs
         Conversion := Read_DR and (2**12 - 1);

         -- Convert value from ADC to mm
         Pos_mm := ADC_To_Position (Conversion);

         -- Write the conversion result
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

         Write_CR (Get_CR (Trigger => True));
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
