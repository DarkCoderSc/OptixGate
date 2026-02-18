object ControlFormDumpProcess: TControlFormDumpProcess
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  BorderIcons = [biSystemMenu]
  Caption = 'Dump Process'
  ClientHeight = 528
  ClientWidth = 505
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
  object PanelMain: TFlatPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 528
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = 13554645
    BorderColor = clBlack
    ExplicitWidth = 495
    ExplicitHeight = 1237
    object ButtonCancel: TFlatButton
      Left = 147
      Top = 485
      Width = 80
      Height = 20
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ShowHint = True
      ImageIndex = -1
      Value = 0
      OnClick = ButtonCancelClick
      Busy = False
    end
    object ButtonValidate: TFlatButton
      Left = 233
      Top = 485
      Width = 80
      Height = 20
      Caption = 'Validate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ShowHint = True
      ImageIndex = -1
      Value = 0
      OnClick = ButtonValidateClick
      Busy = False
    end
    object GroupBoxOutputPath: TFlatGroupBox
      AlignWithMargins = True
      Left = 8
      Top = 388
      Width = 489
      Height = 94
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Output File Path'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Color = 13554645
      Padding.Left = 8
      Padding.Top = 20
      Padding.Right = 8
      Padding.Bottom = 8
      ParentColor = False
      TabOrder = 2
      ExplicitTop = 381
      ExplicitWidth = 479
      object RadioTempFile: TFlatCheckBox
        Left = 8
        Top = 20
        Width = 473
        Height = 17
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        Caption = 'Temporary File (Random File Name in Temp Directory)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Mode = cbmRadioBox
        Checked = True
        OnStateChanged = RadioTempFileStateChanged
        ExplicitWidth = 463
      end
      object RadioCutomFileName: TFlatCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 41
        Width = 473
        Height = 17
        Margins.Left = 0
        Margins.Top = 4
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Custom File Path:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Mode = cbmRadioBox
        Checked = False
        OnStateChanged = RadioCutomFileNameStateChanged
        ExplicitWidth = 463
      end
      object EditCustomFilePath: TFlatEdit
        Left = 8
        Top = 58
        Width = 473
        Height = 21
        Margins.Left = 25
        Margins.Top = 4
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        AutoSize = False
        Color = clWhite
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Status = cStatusNormal
        Validators = []
        ShowBorder = True
        ExplicitWidth = 463
      end
    end
    object GroupDumpTypes: TFlatGroupBox
      AlignWithMargins = True
      Left = 8
      Top = 77
      Width = 489
      Height = 303
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Mini Dump Types'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Color = 13554645
      Padding.Left = 8
      Padding.Top = 20
      Padding.Right = 8
      Padding.Bottom = 8
      ParentColor = False
      TabOrder = 0
      ExplicitTop = 70
      ExplicitWidth = 479
      object PanelRightTypes: TFlatPanel
        Left = 249
        Top = 20
        Width = 232
        Height = 275
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        BorderTop = 0
        BorderLeft = 0
        BorderRight = 0
        BorderBottom = 0
        Color = 13554645
        BorderColor = clBlack
        ExplicitLeft = 268
        ExplicitWidth = 207
        ExplicitHeight = 279
        object CheckBoxMiniDumpWithThreadInfo: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 232
          Height = 17
          Hint = 'Include thread state information.'
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithThreadInfo'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithCodeSegs: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 21
          Width = 232
          Height = 17
          Hint = 
            'Include all code and code-related sections from loaded modules t' +
            'o capture executable content. '
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithCodeSegs'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithoutAuxiliaryState: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 42
          Width = 232
          Height = 17
          Hint = 'Turns off secondary auxiliary-supported memory gathering.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithoutAuxiliaryState'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithFullAuxiliaryState: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 63
          Width = 232
          Height = 17
          Hint = 
            'Requests that auxiliary data providers include their state in th' +
            'e dump image; the state data that is'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithFullAuxiliaryState'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithPrivateWriteCopyMemory: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 84
          Width = 232
          Height = 17
          Hint = 
            'Scans the virtual address space for PAGE_WRITECOPY memory to be ' +
            'included.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithPrivateWriteCopyMemory'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpIgnoreInaccessibleMemory: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 105
          Width = 232
          Height = 17
          Hint = 
            'If you specify MiniDumpWithFullMemory, the MiniDumpWriteDump fun' +
            'ction will fail if the function cannot read the memory regions; ' +
            'however,'#13#10' if you include MiniDumpIgnoreInaccessibleMemory, the ' +
            'MiniDumpWriteDump function will ignore the memory read failures ' +
            'and continue to'#13#10' generate the dump. Note that the inaccessible ' +
            'memory regions are not included in the dump.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpIgnoreInaccessibleMemory'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithTokenInformation: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 126
          Width = 232
          Height = 17
          Hint = 
            'Adds security token related data. This will make the "!token" ex' +
            'tension work when'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithTokenInformation'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithModuleHeaders: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 147
          Width = 232
          Height = 17
          Hint = 'Adds module header related data.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithModuleHeaders'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpFilterTriage: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 168
          Width = 232
          Height = 17
          Hint = 'Adds filter triage related data.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpFilterTriage'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithAvxXStateContext: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 189
          Width = 232
          Height = 17
          Hint = 'Adds AVX crash state context registers.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithAvxXStateContext'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpWithIptTrace: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 210
          Width = 232
          Height = 17
          Hint = 'Adds Intel Processor Trace related data.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithIptTrace'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpScanInaccessiblePartialPages: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 231
          Width = 232
          Height = 17
          Hint = 'Scans inaccessible partial memory pages.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpScanInaccessiblePartialPages'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
        object CheckBoxMiniDumpValidTypeFlags: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 252
          Width = 232
          Height = 17
          Hint = 'Indicates which flags are valid.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpValidTypeFlags'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 207
        end
      end
      object PanelLeftTypes: TFlatPanel
        Left = 8
        Top = 20
        Width = 241
        Height = 275
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        BorderTop = 0
        BorderLeft = 0
        BorderRight = 0
        BorderBottom = 0
        Color = 13554645
        BorderColor = clBlack
        object CheckBoxMiniDumpNormal: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 241
          Height = 17
          Hint = 
            ' Include just the information necessary to capture stack traces ' +
            'for all existing threads in a process.'
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpNormal'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithDataSegs: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 21
          Width = 241
          Height = 17
          Hint = 
            'Include the data sections from all loaded modules. This results ' +
            'in the inclusion of global variables, which'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithDataSegs'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithFullMemory: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 42
          Width = 241
          Height = 17
          Hint = 
            'Include all accessible memory in the process. The raw memory dat' +
            'a is included at the end, so that the'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithFullMemory'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithHandleData: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 63
          Width = 241
          Height = 17
          Hint = 
            'Include high-level information about the operating system handle' +
            's that are active when the minidump is'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithHandleData'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpScanMemory: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 105
          Width = 241
          Height = 17
          Hint = 
            'Stack and backing store memory should be scanned for pointer ref' +
            'erences to modules in the module list.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpScanMemory'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpFilterMemory: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 84
          Width = 241
          Height = 17
          Hint = 
            'Stack and backing store memory written to the minidump file shou' +
            'ld be filtered to remove all but the'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpFilterMemory'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithUnloadedModules: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 126
          Width = 241
          Height = 17
          Hint = 
            'Include information from the list of modules that were recently ' +
            'unloaded, if this information is maintained'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithUnloadedModules'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithIndirectlyReferencedMemory: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 147
          Width = 241
          Height = 17
          Hint = 
            'Include pages with data referenced by locals or other stack memo' +
            'ry. This option can increase the size of'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithIndirectlyReferencedMemory'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpFilterModulePaths: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 168
          Width = 241
          Height = 17
          Hint = 
            'Filter module paths for information such as user names or import' +
            'ant directories. This option may prevent'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpFilterModulePaths'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithProcessThreadData: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 189
          Width = 241
          Height = 17
          Hint = 
            'Include complete per-process and per-thread information from the' +
            ' operating system.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithProcessThreadData'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithPrivateReadWriteMemory: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 210
          Width = 241
          Height = 17
          Hint = 
            'Scan the virtual address space for PAGE_READWRITE memory to be i' +
            'ncluded.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithPrivateReadWriteMemory'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithoutOptionalData: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 231
          Width = 241
          Height = 17
          Hint = 
            'Reduce the data that is dumped by eliminating memory regions tha' +
            't are not essential to meet criteria'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithoutOptionalData'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = False
          ExplicitWidth = 264
        end
        object CheckBoxMiniDumpWithFullMemoryInfo: TFlatCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 252
          Width = 241
          Height = 17
          Hint = 'Include memory region information.'
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'MiniDumpWithFullMemoryInfo'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Mode = cbmCheckBox
          Checked = True
          ExplicitWidth = 264
        end
      end
    end
    object PanelHeaderInfo: TFlatPanel
      Left = 8
      Top = 8
      Width = 489
      Height = 61
      Margins.Left = 8
      Margins.Top = 16
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Padding.Left = 8
      Padding.Top = 8
      Padding.Right = 8
      Padding.Bottom = 8
      BorderTop = 1
      BorderLeft = 1
      BorderRight = 1
      BorderBottom = 1
      Color = 13554645
      BorderColor = clBlack
      object Image: TSkSvg
        Left = 8
        Top = 8
        Width = 41
        Height = 45
        Align = alLeft
        Svg.Source = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path d="M0.5 3C0.5 2.17157 1.1' +
          '7157 1.5 2 1.5H14C14.8284 1.5 15.5 2.17157 15.5 3V14C15.5 14.276' +
          '1 15.2761 14.5 15 14.5H1C0.723858 14.5 0.5 14.2761 0.5 14V3Z" fi' +
          'll="#92B9E8" stroke="url(#paint0_linear_85_1730)"/>'#13#10'<g filter="' +
          'url(#filter0_i_85_1730)">'#13#10'<path d="M1 3H15V14H1V3Z" fill="url(#' +
          'paint1_radial_85_1730)"/>'#13#10'</g>'#13#10'<rect x="1" y="3" width="14" he' +
          'ight="1" fill="url(#paint2_linear_85_1730)"/>'#13#10'<rect x="14" y="2' +
          '" width="1" height="1" fill="#FF6444"/>'#13#10'<rect x="12" y="2" widt' +
          'h="1" height="1" fill="white"/>'#13#10'<rect width="1" height="0.5" tr' +
          'ansform="matrix(1 0 0 -1 10 3)" fill="white"/>'#13#10'<defs>'#13#10'<filter ' +
          'id="filter0_i_85_1730" x="1" y="3" width="14" height="11" filter' +
          'Units="userSpaceOnUse" color-interpolation-filters="sRGB">'#13#10'<feF' +
          'lood flood-opacity="0" result="BackgroundImageFix"/>'#13#10'<feBlend m' +
          'ode="normal" in="SourceGraphic" in2="BackgroundImageFix" result=' +
          '"shape"/>'#13#10'<feColorMatrix in="SourceAlpha" type="matrix" values=' +
          '"0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 127 0" result="hardAlpha"/>' +
          #13#10'<feOffset dx="-1" dy="2"/>'#13#10'<feComposite in2="hardAlpha" opera' +
          'tor="arithmetic" k2="-1" k3="1"/>'#13#10'<feColorMatrix type="matrix" ' +
          'values="0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0"/>'#13#10'<feBlend mod' +
          'e="normal" in2="shape" result="effect1_innerShadow_85_1730"/>'#13#10'<' +
          '/filter>'#13#10'<linearGradient id="paint0_linear_85_1730" x1="0" y1="' +
          '1" x2="16.1312" y2="14.8472" gradientUnits="userSpaceOnUse">'#13#10'<s' +
          'top stop-color="#7BA7DC"/>'#13#10'<stop offset="1" stop-color="#5171A9' +
          '"/>'#13#10'</linearGradient>'#13#10'<radialGradient id="paint1_radial_85_173' +
          '0" cx="0" cy="0" r="1" gradientUnits="userSpaceOnUse" gradientTr' +
          'ansform="translate(15 3) rotate(90) scale(11 14)">'#13#10'<stop stop-c' +
          'olor="#DEE7F2"/>'#13#10'<stop offset="1" stop-color="white"/>'#13#10'</radia' +
          'lGradient>'#13#10'<linearGradient id="paint2_linear_85_1730" x1="1" y1' +
          '="3.5" x2="15" y2="3.5" gradientUnits="userSpaceOnUse">'#13#10'<stop s' +
          'top-color="#7BA7DC"/>'#13#10'<stop offset="1" stop-color="#7BA7DC"/>'#13#10 +
          '</linearGradient>'#13#10'</defs>'#13#10'</svg>'
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
  end
end
