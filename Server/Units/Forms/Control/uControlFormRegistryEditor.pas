{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                             Project: Optix Gate                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://bsky.app/profile/darkcodersc.bsky.social           }
{                   https://github.com/darkcodersc                             }
{                   License: GPL v3                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{                                                                              }
{  Authorship (No AI):                                                         }
{  -------------------                                                         }
{   All code contained in this unit was written and developed by the author    }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{******************************************************************************}



unit uControlFormRegistryEditor;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus,

  Optix.Registry.Helper, Optix.Shared.Classes,

  __uBaseFormControl__, uFrameHexEditor;
// ---------------------------------------------------------------------------------------------------------------------

type
  TControlFormRegistryEditor = class(TBaseFormControl)
    Notebook: TNotebook;
    PanelSZ: TPanel;
    Label1: TLabel;
    EditSZ: TEdit;
    PanelMSZ: TPanel;
    Label2: TLabel;
    RichMSZ: TRichEdit;
    PanelQDword: TPanel;
    Label3: TLabel;
    EditQDword: TEdit;
    GroupBase: TGroupBox;
    RadioBaseDecimal: TRadioButton;
    RadioBaseHexadecimal: TRadioButton;
    PanelBinary: TPanel;
    PanelFooter: TPanel;
    ButtonAction: TButton;
    ButtonCancel: TButton;
    PanelHeader: TPanel;
    LabelName: TLabel;
    EditName: TEdit;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    Switch1: TMenuItem;
    String1: TMenuItem;
    MultiLineString1: TMenuItem;
    DWORD1: TMenuItem;
    QWORD1: TMenuItem;
    Binary1: TMenuItem;
    procedure EditQDwordKeyPress(Sender: TObject; var Key: Char);
    procedure RadioBaseDecimalClick(Sender: TObject);
    procedure RadioBaseHexadecimalClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonActionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure String1Click(Sender: TObject);
    procedure MultiLineString1Click(Sender: TObject);
    procedure DWORD1Click(Sender: TObject);
    procedure QWORD1Click(Sender: TObject);
    procedure Binary1Click(Sender: TObject);
    procedure EditNameKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FManagerGUID    : TGUID;
    FFullKeyPath    : String;
    FValueKind      : DWORD;
    FFrameHexEditor : TFrameHexEditor;
    FEditMode       : Boolean;

    {@M}
    procedure UpdateQDWordValueBase();
    procedure SetEditMode(const AValue : Boolean);
    procedure SetValueKind(const AValue : DWORD);
    procedure SetFullKeyPath(const AValue : String);
    procedure DoResize();
  protected
    function GetContextDescription() : String; override;
    procedure RefreshCaption(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String;
      const ASpecialForm : Boolean = False); override;
    destructor Destroy(); override;

    {@M}
    procedure SetData(const pData : Pointer; const ADataSize : UInt64); overload;
    procedure SetData(const AValue : TOptixMemoryObject); overload;

    {@G/S}
    property EditMode    : Boolean read FEditMode    write SetEditMode;
    property ValueKind   : DWORD   read FValueKind   write SetValueKind;
    property FullKeyPath : String  read FFullKeyPath write SetFullKeyPath;
    property ManagerGUID : TGUID   read FManagerGUID write FManagerGUID;
  end;

var
  ControlFormRegistryEditor: TControlFormRegistryEditor;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math, System.StrUtils,

  Optix.Func.Commands.Registry, Optix.Shared.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormRegistryEditor.SetData(const pData : Pointer; const ADataSize : UInt64);
begin
  if not Assigned(pData) or (ADataSize = 0) then
    Exit();
  ///

  case FValueKind of
    REG_SZ :
      EditSZ.Text := TMemoryUtils.MemoryToString(pData, ADataSize);

    REG_MULTI_SZ :
      RichMSZ.Text := TMemoryUtils.MemoryMultiStringToString(pData, ADataSize);

    REG_DWORD, REG_QWORD : begin
      RadioBaseDecimal.Checked := True;

      if FValueKind = REG_DWORD then
        EditQDWord.Text := UIntToStr(PDWORD(pData)^)
      else
        EditQDWord.Text := UIntToStr(PUInt64(pData)^)
    end;

    REG_BINARY : begin
      if Assigned(FFrameHexEditor) then
        FFrameHexEditor.LoadData(pData, ADataSize);
    end;
  end;
end;

procedure TControlFormRegistryEditor.SetData(const AValue : TOptixMemoryObject);
begin
  if not Assigned(AValue) or not AValue.HasData then
    Exit();
  ///

  SetData(AValue.Address, AValue.Size);
end;

function TControlFormRegistryEditor.GetContextDescription() : String;
begin
  result := '';
  ///

  if FEditMode then
   result := 'Editing '
  else
   result := 'Creating New ';

  result := result + TRegistryHelper.ValueKindToString(FValueKind);
end;

procedure TControlFormRegistryEditor.MultiLineString1Click(Sender: TObject);
begin
  SetValueKind(REG_MULTI_SZ);
end;

procedure TControlFormRegistryEditor.QWORD1Click(Sender: TObject);
begin
  SetValueKind(REG_QWORD);
end;

procedure TControlFormRegistryEditor.RefreshCaption();
begin
  inherited;
  ///

  Caption := Caption + ' - ' + GetContextDescription();
end;

procedure TControlFormRegistryEditor.DoResize();
begin
  var ANewH := -1;
  var ANewW := -1;
  ///

  case FValueKind of
    REG_SZ : begin
      ANewH := EditSZ.Top + EditSZ.Height;
      ANewW := ScaleValue(400);
    end;

    REG_MULTI_SZ : begin
      ANewH := RichMSZ.Top + ScaleValue(200);
      ANewW := ScaleValue(500);
    end;

    REG_DWORD, REG_QWORD : begin
      ANewH := GroupBase.Top + GroupBase.Height;
      ANewW := ScaleValue(400);
    end;

    REG_BINARY : begin
      ANewH := FFrameHexEditor.Top + ScaleValue(200);
      ANewW := ScaleValue(750);
    end;
  end;

  if ANewH > 0 then begin
    Inc(ANewH, PanelHeader.Height + PanelFooter.Height + StatusBar.Height);

    ///
    ClientHeight := ANewH;
  end;

  if ANewW > 0 then
    ClientWidth := ANewW;
end;

procedure TControlFormRegistryEditor.DWORD1Click(Sender: TObject);
begin
  SetValueKind(REG_DWORD);
end;

procedure TControlFormRegistryEditor.Binary1Click(Sender: TObject);
begin
  SetValueKind(REG_BINARY);
end;

procedure TControlFormRegistryEditor.ButtonActionClick(Sender: TObject);
begin
  if (FValueKind = REG_NONE) or String.IsNullOrWhiteSpace(FFullKeyPath) then
    Exit();
  ///

  if String.IsNullOrWhiteSpace(EditName.Text) and not FEditMode then begin
    EditName.SetFocus;

    raise Exception.Create('You must specify a registry value name.');
  end;

  var pData := nil;
  var ADataSize := UInt64(0);

  case FValueKind of
    REG_SZ       : TMemoryUtils.StringToMemory(EditSZ.Text, pData, ADataSize);
    REG_MULTI_SZ : TMemoryUtils.StringToMemory(RichMSZ.Text, pData, ADataSize);

    REG_DWORD, REG_QWORD : begin
      var AValue : UInt64;
      if not TryStrToUInt64(IfThen(RadioBaseHexadecimal.Checked, '$', '') + EditQDword.Text, AValue) then
        Exit();

      if FValueKind = REG_DWORD then
        TMemoryUtils.DwordToMemory(DWORD(AValue), pData, ADataSize)
      else
        TMemoryUtils.QwordToMemory(AValue, pData, ADataSize);
    end;

    REG_BINARY : begin
      if Assigned(FFrameHexEditor) then begin
        pData     := FFrameHexEditor.Data;
        ADataSize := FFrameHexEditor.DataSize;
      end;
    end;
  end;

  SendCommand(
    TOptixCommandSetRegistryValue.Create(FFullKeyPath, EditName.Text, FValueKind, pData, ADataSize),
    FManagerGUID
  );

  ///
  Close;
end;

procedure TControlFormRegistryEditor.ButtonCancelClick(Sender: TObject);
begin
//  if Application.MessageBox(
//    'You are about to close this form and permanently lose any unsaved work. Are you sure you want to proceed?',
//    'Registry Editor',
//    MB_ICONQUESTION + MB_YESNO
//  ) = ID_NO then
//    Exit();
//  ///

  Close;
end;

constructor TControlFormRegistryEditor.Create(AOwner : TComponent; const AUserIdentifier : String;
  const ASpecialForm : Boolean = False);
begin
  inherited;
  ///

  FFullKeyPath := '';
  SetEditMode(False);

  FFrameHexEditor := TFrameHexEditor.Create(PanelBinary);
  FFrameHexEditor.Parent := PanelBinary;
  FFrameHexEditor.Align := alClient;

  FValueKind := REG_NONE;
end;

destructor TControlFormRegistryEditor.Destroy();
begin
  if Assigned(FFrameHexEditor) then
    FreeAndNil(FFrameHexEditor);

  ///
  inherited Destroy();
end;

procedure TControlFormRegistryEditor.UpdateQDWordValueBase();
begin
  if (FValueKind <> REG_DWORD) and (FValueKind <> REG_QWORD) then
    Exit();
  try
    var AValue : UInt64;
    if not TryStrToUInt64(IfThen(RadioBaseDecimal.Checked, '$', '') + EditQDword.Text, AValue) then
      Exit();

    if RadioBaseDecimal.Checked then
      EditQDWord.Text := UIntToStr(AValue)
    else
      EditQDWord.Text := Format('%x', [AValue]);
  finally
    EditQDword.SetFocus;
    EditQDword.SelStart := Length(EditQDword.Text);
  end;
end;

procedure TControlFormRegistryEditor.EditNameKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
    ButtonActionClick(ButtonAction);
end;

procedure TControlFormRegistryEditor.EditQDwordKeyPress(Sender: TObject; var Key: Char);
begin
  if ((FValueKind <> REG_DWORD) and (FValueKind <> REG_QWORD)) or (Key < #32) then
    Exit();
  ///

  var AEditFinalValue := Copy(TEdit(Sender).Text, 1, TEdit(Sender).SelStart) +
                         Key +
                         Copy(TEdit(Sender).Text, TEdit(Sender).SelStart + TEdit(Sender).SelLength + 1, MaxInt);

  var ACandidate := Key;
  Key := #0;
  ///

  if CharInSet(ACandidate, ['a'..'f']) then
    ACandidate := UpCase(ACandidate);

  var AValue : UInt64;
  if not TryStrToUInt64(IfThen(RadioBaseHexadecimal.Checked, '$', '') + AEditFinalValue, AValue) then
    Exit();

  if (FValueKind = REG_DWORD) and (AValue > High(DWORD)) or ((FValueKind = REG_QWORD) and (AValue > High(UInt64))) then
    Exit();

  ///
  Key := ACandidate;
end;

procedure TControlFormRegistryEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TControlFormRegistryEditor.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FValueKind = REG_BINARY) or (FValueKind = REG_MULTI_SZ) then
    Exit();
  ///

  case Key of
    13 : ButtonActionClick(ButtonAction);
    27 : ButtonCancelClick(ButtonCancel);
  end;
end;

procedure TControlFormRegistryEditor.FormShow(Sender: TObject);
begin
  if EditName.Enabled then
    EditName.SetFocus
  else begin
    case FValueKind of
      REG_SZ       : EditSZ.SetFocus;
      REG_MULTI_SZ : RichMSZ.SetFocus;
      REG_DWORD,
      REG_QWORD    : EditQDword.SetFocus;
    end;
  end;
  ///

  DoResize();
end;

procedure TControlFormRegistryEditor.RadioBaseDecimalClick(Sender: TObject);
begin
  UpdateQDWordValueBase();
end;

procedure TControlFormRegistryEditor.RadioBaseHexadecimalClick(Sender: TObject);
begin
  UpdateQDWordValueBase();
end;

procedure TControlFormRegistryEditor.SetEditMode(const AValue : Boolean);
begin
  FEditMode := AValue;

  EditName.Enabled := not FEditMode;

  if FEditMode then
    ButtonAction.Caption := 'Save'
  else
    ButtonAction.Caption := 'Create';
end;

procedure TControlFormRegistryEditor.SetValueKind(const AValue : DWORD);
// TODO: Simplify this touchy method
begin
  if AValue = FValueKind then
    Exit();
  ///

  var AOldValueKind := FValueKind;
  FValueKind := AValue;

  var ANoteBookIndex := 0;

  case FValueKind of
    REG_SZ : begin
      case AOldValueKind of
        REG_MULTI_SZ :
          EditSZ.Text := StringReplace(RichMSZ.Text, #13#10, '\0', [rfReplaceAll]);

        REG_DWORD, REG_QWORD :
          EditSZ.Text := EditQDWord.Text;

        REG_BINARY :
          EditSZ.Text := TContentFormater.ExtractStrings(
            FFrameHexEditor.Data,
            FFrameHexEditor.DataSize
          );
        else
          EditSZ.Clear();
      end;

      ///
      ANoteBookIndex := 0;
    end;

    REG_MULTI_SZ : begin
      case AOldValueKind of
        REG_SZ :
          RichMSZ.Text := EditSZ.Text;

        REG_DWORD, REG_QWORD :
          RichMSZ.Text := EditQDWord.Text;

        REG_BINARY :
          RichMSZ.Text := TContentFormater.ExtractStrings(
            FFrameHexEditor.Data,
            FFrameHexEditor.DataSize
          );
        else
          RichMSZ.Clear();
      end;

      ///
      ANoteBookIndex := 1;
    end;

    REG_DWORD, REG_QWORD : begin
      RadioBaseDecimal.Checked := True;
      ///

      case AOldValueKind of
        REG_SZ, REG_MULTI_SZ : begin
          EditQDword.Text := '0';

          var AString : String;
          if AOldValueKind = REG_SZ then
            AString := EditSZ.Text
          else
            AString := RichMSZ.Text;

          if FValueKind = REG_DWORD then begin
            var ACandidate : DWORD;
            if TryStrToUInt(AString.Trim(), ACandidate) then
              EditQDWord.Text := AString.Trim();
          end else begin
            var ACandidate : UInt64;
            if TryStrToUInt64(AString.Trim(), ACandidate) then
              EditQDWord.Text := AString.Trim();
          end;
        end;

        REG_DWORD: ;

        REG_QWORD : begin
          var ACandidate : UInt64;
          if TryStrToUInt64(EditQDWord.Text, ACandidate) then
            EditQDWord.Text := UIntToStr(DWORD(ACandidate));
        end;

        REG_BINARY : begin
          if (FValueKind = REG_DWORD) and (FFrameHexEditor.DataSize >= SizeOf(DWORD)) then
            EditQDWord.Text := UIntToStr(PDWORD(FFrameHexEditor.Data)^)
          else if (FVAlueKind = REG_QWORD) and (FFrameHexEditor.DataSize >= SizeOf(UInt64)) then
            EditQDWord.Text := UIntToStr(PUInt64(FFrameHexEditor.Data)^)
        end;

        else
          EditQDWord.Text := '0';
      end;

      ///
      ANoteBookIndex := 2;
    end;

    REG_BINARY : begin
      case AOldValueKind of
        REG_SZ :
          FFrameHexEditor.LoadData(PWideChar(EditSZ.Text), Length(EditSZ.Text) * SizeOf(WideChar));

        REG_MULTI_SZ :
          FFrameHexEditor.LoadData(PWideChar(RichMSZ.Text), Length(RichMSZ.Text) * SizeOf(WideChar));

        REG_DWORD : begin
          var ACandidate : DWORD;
          if TryStrToUInt(EditQDWord.Text, ACandidate) then
            FFrameHexEditor.LoadData(@ACandidate, SizeOf(DWORD));
        end;

        REG_QWORD : begin
          var ACandidate : UInt64;
          if TryStrToUInt64(EditQDWord.Text, ACandidate) then
            FFrameHexEditor.LoadData(@ACandidate, SizeOf(UInt64));
        end;
        else
          FFrameHexEditor.Clear();
      end;

      ///
      ANoteBookIndex := 3;
    end;
  end;

  Notebook.PageIndex := ANoteBookIndex;

  String1.Enabled          := FValueKind <> REG_SZ;
  MultiLineString1.Enabled := FValueKind <> REG_MULTI_SZ;
  DWORD1.Enabled           := FValueKind <> REG_DWORD;
  QWORD1.Enabled           := FValueKind <> REG_QWORD;
  Binary1.Enabled          := FValueKind <> REG_BINARY;

  RefreshCaption();

  ///
  DoResize();
end;

procedure TControlFormRegistryEditor.String1Click(Sender: TObject);
begin
  SetValueKind(REG_SZ);
end;

procedure TControlFormRegistryEditor.SetFullKeyPath(const AValue : String);
begin
  if FFullKeyPath = AValue then
    Exit();
  ///

  FFullKeyPath := AValue;

  StatusBar.Panels[0].Text := FFullKeyPath;
end;

end.
