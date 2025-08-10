object FormDumpProcess: TFormDumpProcess
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  BorderStyle = bsDialog
  Caption = 'Dump Process'
  ClientHeight = 580
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  ShowHint = True
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object GroupDumpTypes: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 62
    Width = 532
    Height = 345
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Caption = 'Mini Dump Types'
    TabOrder = 0
    ExplicitWidth = 522
    object PanelRightTypes: TPanel
      Left = 266
      Top = 17
      Width = 264
      Height = 326
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 254
      object CheckBoxMiniDumpWithThreadInfo: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 4
        Width = 253
        Height = 17
        Hint = 'Include thread state information.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithThreadInfo'
        Checked = True
        State = cbChecked
        TabOrder = 0
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithCodeSegs: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 28
        Width = 253
        Height = 17
        Hint = 
          'Include all code and code-related sections from loaded modules t' +
          'o capture executable content. '
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithCodeSegs'
        TabOrder = 1
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithoutAuxiliaryState: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 52
        Width = 253
        Height = 17
        Hint = 'Turns off secondary auxiliary-supported memory gathering.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithoutAuxiliaryState'
        TabOrder = 2
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithFullAuxiliaryState: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 76
        Width = 253
        Height = 17
        Hint = 
          'Requests that auxiliary data providers include their state in th' +
          'e dump image; the state data that is'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithFullAuxiliaryState'
        Checked = True
        State = cbChecked
        TabOrder = 3
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithPrivateWriteCopyMemory: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 100
        Width = 253
        Height = 17
        Hint = 
          'Scans the virtual address space for PAGE_WRITECOPY memory to be ' +
          'included.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithPrivateWriteCopyMemory'
        TabOrder = 4
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpIgnoreInaccessibleMemory: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 124
        Width = 253
        Height = 17
        Hint = 
          'If you specify MiniDumpWithFullMemory, the MiniDumpWriteDump fun' +
          'ction will fail if the function cannot read the memory regions; ' +
          'however,'#13#10' if you include MiniDumpIgnoreInaccessibleMemory, the ' +
          'MiniDumpWriteDump function will ignore the memory read failures ' +
          'and continue to'#13#10' generate the dump. Note that the inaccessible ' +
          'memory regions are not included in the dump.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpIgnoreInaccessibleMemory'
        Checked = True
        State = cbChecked
        TabOrder = 5
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithTokenInformation: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 148
        Width = 253
        Height = 17
        Hint = 
          'Adds security token related data. This will make the "!token" ex' +
          'tension work when'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithTokenInformation'
        Checked = True
        State = cbChecked
        TabOrder = 6
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithModuleHeaders: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 172
        Width = 253
        Height = 17
        Hint = 'Adds module header related data.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithModuleHeaders'
        TabOrder = 7
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpFilterTriage: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 196
        Width = 253
        Height = 17
        Hint = 'Adds filter triage related data.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpFilterTriage'
        TabOrder = 8
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithAvxXStateContext: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 220
        Width = 253
        Height = 17
        Hint = 'Adds AVX crash state context registers.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithAvxXStateContext'
        TabOrder = 9
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpWithIptTrace: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 244
        Width = 253
        Height = 17
        Hint = 'Adds Intel Processor Trace related data.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithIptTrace'
        TabOrder = 10
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpScanInaccessiblePartialPages: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 268
        Width = 253
        Height = 17
        Hint = 'Scans inaccessible partial memory pages.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpScanInaccessiblePartialPages'
        TabOrder = 11
        ExplicitWidth = 243
      end
      object CheckBoxMiniDumpValidTypeFlags: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 292
        Width = 253
        Height = 17
        Hint = 'Indicates which flags are valid.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpValidTypeFlags'
        TabOrder = 12
        ExplicitWidth = 243
      end
    end
    object PanelLeftTypes: TPanel
      Left = 2
      Top = 17
      Width = 264
      Height = 326
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object CheckBoxMiniDumpNormal: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 253
        Height = 17
        Hint = 
          ' Include just the information necessary to capture stack traces ' +
          'for all existing threads in a process.'
        Margins.Left = 8
        Margins.Top = 8
        Align = alTop
        Caption = 'MiniDumpNormal'
        TabOrder = 0
      end
      object CheckBoxMiniDumpWithDataSegs: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 32
        Width = 253
        Height = 17
        Hint = 
          'Include the data sections from all loaded modules. This results ' +
          'in the inclusion of global variables, which'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithDataSegs'
        TabOrder = 1
      end
      object CheckBoxMiniDumpWithFullMemory: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 56
        Width = 253
        Height = 17
        Hint = 
          'Include all accessible memory in the process. The raw memory dat' +
          'a is included at the end, so that the'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithFullMemory'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object CheckBoxMiniDumpWithHandleData: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 80
        Width = 253
        Height = 17
        Hint = 
          'Include high-level information about the operating system handle' +
          's that are active when the minidump is'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithHandleData'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object CheckBoxMiniDumpScanMemory: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 128
        Width = 253
        Height = 17
        Hint = 
          'Stack and backing store memory should be scanned for pointer ref' +
          'erences to modules in the module list.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpScanMemory'
        TabOrder = 4
      end
      object CheckBoxMiniDumpFilterMemory: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 104
        Width = 253
        Height = 17
        Hint = 
          'Stack and backing store memory written to the minidump file shou' +
          'ld be filtered to remove all but the'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpFilterMemory'
        TabOrder = 5
      end
      object CheckBoxMiniDumpWithUnloadedModules: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 152
        Width = 253
        Height = 17
        Hint = 
          'Include information from the list of modules that were recently ' +
          'unloaded, if this information is maintained'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithUnloadedModules'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object CheckBoxMiniDumpWithIndirectlyReferencedMemory: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 176
        Width = 253
        Height = 17
        Hint = 
          'Include pages with data referenced by locals or other stack memo' +
          'ry. This option can increase the size of'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithIndirectlyReferencedMemory'
        TabOrder = 7
      end
      object CheckBoxMiniDumpFilterModulePaths: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 200
        Width = 253
        Height = 17
        Hint = 
          'Filter module paths for information such as user names or import' +
          'ant directories. This option may prevent'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpFilterModulePaths'
        TabOrder = 8
      end
      object CheckBoxMiniDumpWithProcessThreadData: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 224
        Width = 253
        Height = 17
        Hint = 
          'Include complete per-process and per-thread information from the' +
          ' operating system.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithProcessThreadData'
        Checked = True
        State = cbChecked
        TabOrder = 9
      end
      object CheckBoxMiniDumpWithPrivateReadWriteMemory: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 248
        Width = 253
        Height = 17
        Hint = 
          'Scan the virtual address space for PAGE_READWRITE memory to be i' +
          'ncluded.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithPrivateReadWriteMemory'
        TabOrder = 10
      end
      object CheckBoxMiniDumpWithoutOptionalData: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 272
        Width = 253
        Height = 17
        Hint = 
          'Reduce the data that is dumped by eliminating memory regions tha' +
          't are not essential to meet criteria'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithoutOptionalData'
        TabOrder = 11
      end
      object CheckBoxMiniDumpWithFullMemoryInfo: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 296
        Width = 253
        Height = 17
        Hint = 'Include memory region information.'
        Margins.Left = 8
        Margins.Top = 4
        Align = alTop
        Caption = 'MiniDumpWithFullMemoryInfo'
        Checked = True
        State = cbChecked
        TabOrder = 12
      end
    end
  end
  object PanelHeaderInfo: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 16
    Width = 532
    Height = 30
    Margins.Left = 8
    Margins.Top = 16
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 522
    object Image: TVirtualImage
      Left = 0
      Top = 0
      Width = 32
      Height = 32
      ImageCollection = FormMain.ImageCollectionDark
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = -1
    end
    object LabelProcessName: TLabel
      Left = 96
      Top = 0
      Width = 78
      Height = 15
      Caption = 'Process Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LabelProcessId: TLabel
      Left = 96
      Top = 20
      Width = 53
      Height = 15
      Caption = 'Process Id'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
  end
  object GroupBoxOutputPath: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 419
    Width = 532
    Height = 110
    Margins.Left = 8
    Margins.Top = 4
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Caption = 'Output File Path'
    TabOrder = 2
    ExplicitWidth = 522
    object RadioTempFile: TRadioButton
      AlignWithMargins = True
      Left = 10
      Top = 25
      Width = 512
      Height = 17
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Caption = 'Temporary File (Random File Name in Temp Directory)'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioTempFileClick
      ExplicitWidth = 502
    end
    object RadioCutomFileName: TRadioButton
      AlignWithMargins = True
      Left = 10
      Top = 50
      Width = 512
      Height = 17
      Margins.Left = 8
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Custom File Path'
      TabOrder = 1
      OnClick = RadioCutomFileNameClick
      ExplicitWidth = 502
    end
    object EditCustomFilePath: TEdit
      AlignWithMargins = True
      Left = 27
      Top = 71
      Width = 495
      Height = 23
      Margins.Left = 25
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Enabled = False
      TabOrder = 2
      ExplicitWidth = 485
    end
  end
  object ButtonCancel: TButton
    Left = 154
    Top = 540
    Width = 89
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object ButtonValidate: TButton
    Left = 249
    Top = 540
    Width = 89
    Height = 25
    Caption = 'Validate'
    TabOrder = 4
    OnClick = ButtonValidateClick
  end
end
