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
{  All code contained in this unit was written and developed by the author     }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{   The author grants permission for this code to be used, reproduced, and     }
{   included in datasets for the purpose of training or improving machine      }
{   learning models, including large language models (LLMs).                   }
{                                                                              }
{******************************************************************************}

unit uControlFormRegistryEditor;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,

  Optix.Registry.Helper,

  __uBaseFormControl__, uFrameHexEditor, Vcl.Menus;
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
    procedure Reset();
  protected
    function GetContextDescription() : String; override;
    procedure RefreshCaption(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String;
      const ASpecialForm : Boolean = False); override;
    destructor Destroy(); override;

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

  Optix.Func.Commands.Registry, Optix.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormRegistryEditor.Reset();
begin
  EditSZ.Clear();
  RichMSZ.Clear();
  EditQDword.Text := '0';

  if Assigned(FFrameHexEditor) then
    FFrameHexEditor.Clear();

  ///
  RefreshCaption();
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
    Inc(ANewH, PanelHeader.Height + PanelFooter.Height + ScaleValue(8) + StatusBar.Height);

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
    TOptixCommandRegistrySetValue.Create(FFullKeyPath, EditName.Text, FValueKind, pData, ADataSize),
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
  if FValueKind = REG_BINARY then
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
begin
  if AValue = FValueKind then
    Exit();
  ///

  FValueKind := AValue;

  var ANoteBookIndex := 0;

  case FValueKind of
    REG_SZ       : ANoteBookIndex := 0;
    REG_MULTI_SZ : ANoteBookIndex := 1;
    REG_DWORD,
    REG_QWORD    : ANoteBookIndex := 2;
    REG_BINARY   : ANoteBookIndex := 3;
  end;

  Notebook.PageIndex := ANoteBookIndex;

  String1.Enabled          := FValueKind <> REG_SZ;
  MultiLineString1.Enabled := FValueKind <> REG_MULTI_SZ;
  DWORD1.Enabled           := FValueKind <> REG_DWORD;
  QWORD1.Enabled           := FValueKind <> REG_QWORD;
  Binary1.Enabled          := FValueKind <> REG_BINARY;

  Reset();

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
