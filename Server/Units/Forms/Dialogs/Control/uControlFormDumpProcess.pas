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
{                                                                              }
{******************************************************************************}

unit uControlFormDumpProcess;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.VirtualImage, Vcl.Mask, __uBaseFormControl__;

type
  TControlFormDumpProcess = class(TForm)
    GroupDumpTypes: TGroupBox;
    PanelRightTypes: TPanel;
    PanelLeftTypes: TPanel;
    CheckBoxMiniDumpNormal: TCheckBox;
    CheckBoxMiniDumpWithDataSegs: TCheckBox;
    CheckBoxMiniDumpWithFullMemory: TCheckBox;
    CheckBoxMiniDumpWithHandleData: TCheckBox;
    CheckBoxMiniDumpScanMemory: TCheckBox;
    CheckBoxMiniDumpFilterMemory: TCheckBox;
    CheckBoxMiniDumpWithUnloadedModules: TCheckBox;
    CheckBoxMiniDumpWithIndirectlyReferencedMemory: TCheckBox;
    CheckBoxMiniDumpFilterModulePaths: TCheckBox;
    CheckBoxMiniDumpWithThreadInfo: TCheckBox;
    CheckBoxMiniDumpWithCodeSegs: TCheckBox;
    CheckBoxMiniDumpWithoutAuxiliaryState: TCheckBox;
    CheckBoxMiniDumpWithFullAuxiliaryState: TCheckBox;
    CheckBoxMiniDumpWithProcessThreadData: TCheckBox;
    CheckBoxMiniDumpWithPrivateReadWriteMemory: TCheckBox;
    CheckBoxMiniDumpWithoutOptionalData: TCheckBox;
    CheckBoxMiniDumpWithPrivateWriteCopyMemory: TCheckBox;
    CheckBoxMiniDumpIgnoreInaccessibleMemory: TCheckBox;
    CheckBoxMiniDumpWithTokenInformation: TCheckBox;
    CheckBoxMiniDumpWithModuleHeaders: TCheckBox;
    CheckBoxMiniDumpFilterTriage: TCheckBox;
    CheckBoxMiniDumpWithAvxXStateContext: TCheckBox;
    CheckBoxMiniDumpWithIptTrace: TCheckBox;
    CheckBoxMiniDumpWithFullMemoryInfo: TCheckBox;
    CheckBoxMiniDumpScanInaccessiblePartialPages: TCheckBox;
    CheckBoxMiniDumpValidTypeFlags: TCheckBox;
    PanelHeaderInfo: TPanel;
    Image: TVirtualImage;
    LabelProcessName: TLabel;
    LabelProcessId: TLabel;
    GroupBoxOutputPath: TGroupBox;
    RadioTempFile: TRadioButton;
    RadioCutomFileName: TRadioButton;
    EditCustomFilePath: TEdit;
    ButtonCancel: TButton;
    ButtonValidate: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RadioCutomFileNameClick(Sender: TObject);
    procedure RadioTempFileClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonValidateClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FProcessId : Cardinal;

    {@M}
    procedure DoResize();
    function GetMiniDumpTypesValue() : DWORD;
  protected
    {@M}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    {@C}
    constructor Create(AOwner : TBaseFormControl; const AName : String; AProcessId : Cardinal; const AUserIdentifier : String; const AImageIndex : Integer); reintroduce;
  end;

var
  ControlFormDumpProcess: TControlFormDumpProcess;

implementation

uses uFormMain, Optix.WinApiEx, Optix.Func.Commands;

{$R *.dfm}

function TControlFormDumpProcess.GetMiniDumpTypesValue() : DWORD;
begin
  result := 0;
  ///

  if CheckBoxMiniDumpNormal.Checked then
    result := result or MiniDumpNormal;

  if CheckBoxMiniDumpWithDataSegs.Checked then
    result := result or MiniDumpWithDataSegs;

  if CheckBoxMiniDumpWithFullMemory.Checked then
    result := result or MiniDumpWithFullMemory;

  if CheckBoxMiniDumpWithHandleData.Checked then
    result := result or MiniDumpWithHandleData;

  if CheckBoxMiniDumpFilterMemory.Checked then
    result := result or MiniDumpFilterMemory;

  if CheckBoxMiniDumpScanMemory.Checked then
    result := result or MiniDumpScanMemory;

  if CheckBoxMiniDumpWithUnloadedModules.Checked then
    result := result or MiniDumpWithUnloadedModules;

  if CheckBoxMiniDumpWithIndirectlyReferencedMemory.Checked then
    result := result or MiniDumpWithIndirectlyReferencedMemory;

  if CheckBoxMiniDumpFilterModulePaths.Checked then
    result := result or MiniDumpFilterModulePaths;

  if CheckBoxMiniDumpWithProcessThreadData.Checked then
    result := result or MiniDumpWithProcessThreadData;

  if CheckBoxMiniDumpWithPrivateReadWriteMemory.Checked then
    result := result or MiniDumpWithPrivateReadWriteMemory;

  if CheckBoxMiniDumpWithoutOptionalData.Checked then
    result := result or MiniDumpWithoutOptionalData;

  if CheckBoxMiniDumpWithFullMemoryInfo.Checked then
    result := result or MiniDumpWithFullMemoryInfo;

  if CheckBoxMiniDumpWithThreadInfo.Checked then
    result := result or MiniDumpWithThreadInfo;

  if CheckBoxMiniDumpWithCodeSegs.Checked then
    result := result or MiniDumpWithCodeSegs;

  if CheckBoxMiniDumpWithoutAuxiliaryState.Checked then
    result := result or MiniDumpWithoutAuxiliaryState;

  if CheckBoxMiniDumpWithFullAuxiliaryState.Checked then
    result := result or MiniDumpWithFullAuxiliaryState;

  if CheckBoxMiniDumpWithPrivateWriteCopyMemory.Checked then
    result := result or MiniDumpWithPrivateWriteCopyMemory;

  if CheckBoxMiniDumpIgnoreInaccessibleMemory.Checked then
    result := result or MiniDumpIgnoreInaccessibleMemory;

  if CheckBoxMiniDumpWithTokenInformation.Checked then
    result := result or MiniDumpWithTokenInformation;

  if CheckBoxMiniDumpWithModuleHeaders.Checked then
    result := result or MiniDumpWithModuleHeaders;

  if CheckBoxMiniDumpFilterTriage.Checked then
    result := result or MiniDumpFilterTriage;

  if CheckBoxMiniDumpWithAvxXStateContext.Checked then
    result := result or MiniDumpWithAvxXStateContext;

  if CheckBoxMiniDumpWithIptTrace.Checked then
    result := result or MiniDumpWithIptTrace;

  if CheckBoxMiniDumpScanInaccessiblePartialPages.Checked then
    result := result or MiniDumpScanInaccessiblePartialPages;

  if CheckBoxMiniDumpValidTypeFlags.Checked then
    result := result or MiniDumpValidTypeFlags;
end;

procedure TControlFormDumpProcess.RadioCutomFileNameClick(Sender: TObject);
begin
  EditCustomFilePath.Enabled := RadioCutomFileName.Checked;
end;

procedure TControlFormDumpProcess.RadioTempFileClick(Sender: TObject);
begin
  RadioCutomFileNameClick(Sender);
end;

procedure TControlFormDumpProcess.ButtonCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TControlFormDumpProcess.ButtonValidateClick(Sender: TObject);
begin
  var ATypesValue := GetMiniDumpTypesValue();

  if ATypesValue = 0 then begin
    Application.MessageBox('You must select at least one mini dump type!', 'Process Dump', MB_ICONHAND);

    Exit();
  end;

  var ADestFilePath := '';

  if RadioCutomFileName.Checked then
    ADestFilePath := EditCustomFilePath.Text;

  TBaseFormControl(Owner).SendCommand(TOptixCommandProcessDump.Create(FProcessId, ADestFilePath, ATypesValue));

  Close();
end;

constructor TControlFormDumpProcess.Create(AOwner : TBaseFormControl; const AName : String; AProcessId : Cardinal; const AUserIdentifier : String; const AImageIndex : Integer);
begin
  inherited Create(AOwner);
  ///

  Caption := Format('%s (%s)', [
    Caption,
    AUserIdentifier
  ]);

  FProcessId := AProcessId;

  LabelProcessName.Caption := AName;
  labelProcessId.Caption   := Format('%d (0x%x)', [FProcessId, FProcessId]);

  Image.ImageIndex := AImageIndex;
end;

procedure TControlFormDumpProcess.DoResize();
begin
  Image.Left := 8;
  Image.Top  := (PanelHeaderInfo.Height div 2) - (Image.Height div 2);

  LabelProcessName.Left := Image.left + Image.Width + 16;
  LabelProcessId.Left   := LabelProcessName.Left;

  LabelProcessName.Top := (PanelHeaderInfo.Height div 2) - (LabelProcessName.Height + 4);
  LabelProcessId.Top   := LabelProcessName.Top + LabelProcessName.Height + 4;

  ButtonCancel.Left := (ClientWidth div 2) - ButtonCancel.Width - 4;
  ButtonValidate.Left := (ClientWidth div 2) + 4;
end;

{ TFormDumpProcess.CreateParams }
procedure TControlFormDumpProcess.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;

procedure TControlFormDumpProcess.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    13 : ButtonValidateClick(ButtonValidate);
    27 : ButtonCancelClick(ButtonCancel);
  end;
end;

procedure TControlFormDumpProcess.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TControlFormDumpProcess.FormShow(Sender: TObject);
begin
  DoResize();
end;

end.
