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

unit uControlFormDumpProcess;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.Skia,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.VirtualImage, Vcl.Mask, Vcl.Skia,

  __uBaseFormControl__,

  NeoFlat.Panel, NeoFlat.GroupBox, NeoFlat.CheckBox, NeoFlat.Edit, NeoFlat.Button;
// ---------------------------------------------------------------------------------------------------------------------

type
  TControlFormDumpProcess = class(TBaseFormControl)
    PanelMain: TFlatPanel;
    ButtonCancel: TFlatButton;
    ButtonValidate: TFlatButton;
    GroupBoxOutputPath: TFlatGroupBox;
    RadioTempFile: TFlatCheckBox;
    RadioCutomFileName: TFlatCheckBox;
    EditCustomFilePath: TFlatEdit;
    GroupDumpTypes: TFlatGroupBox;
    PanelRightTypes: TFlatPanel;
    CheckBoxMiniDumpWithThreadInfo: TFlatCheckBox;
    CheckBoxMiniDumpWithCodeSegs: TFlatCheckBox;
    CheckBoxMiniDumpWithoutAuxiliaryState: TFlatCheckBox;
    CheckBoxMiniDumpWithFullAuxiliaryState: TFlatCheckBox;
    CheckBoxMiniDumpWithPrivateWriteCopyMemory: TFlatCheckBox;
    CheckBoxMiniDumpIgnoreInaccessibleMemory: TFlatCheckBox;
    CheckBoxMiniDumpWithTokenInformation: TFlatCheckBox;
    CheckBoxMiniDumpWithModuleHeaders: TFlatCheckBox;
    CheckBoxMiniDumpFilterTriage: TFlatCheckBox;
    CheckBoxMiniDumpWithAvxXStateContext: TFlatCheckBox;
    CheckBoxMiniDumpWithIptTrace: TFlatCheckBox;
    CheckBoxMiniDumpScanInaccessiblePartialPages: TFlatCheckBox;
    CheckBoxMiniDumpValidTypeFlags: TFlatCheckBox;
    PanelLeftTypes: TFlatPanel;
    CheckBoxMiniDumpNormal: TFlatCheckBox;
    CheckBoxMiniDumpWithDataSegs: TFlatCheckBox;
    CheckBoxMiniDumpWithFullMemory: TFlatCheckBox;
    CheckBoxMiniDumpWithHandleData: TFlatCheckBox;
    CheckBoxMiniDumpScanMemory: TFlatCheckBox;
    CheckBoxMiniDumpFilterMemory: TFlatCheckBox;
    CheckBoxMiniDumpWithUnloadedModules: TFlatCheckBox;
    CheckBoxMiniDumpWithIndirectlyReferencedMemory: TFlatCheckBox;
    CheckBoxMiniDumpFilterModulePaths: TFlatCheckBox;
    CheckBoxMiniDumpWithProcessThreadData: TFlatCheckBox;
    CheckBoxMiniDumpWithPrivateReadWriteMemory: TFlatCheckBox;
    CheckBoxMiniDumpWithoutOptionalData: TFlatCheckBox;
    CheckBoxMiniDumpWithFullMemoryInfo: TFlatCheckBox;
    PanelHeaderInfo: TFlatPanel;
    Image: TSkSvg;
    LabelProcessName: TLabel;
    LabelProcessId: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonValidateClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RadioTempFileStateChanged(Sender: TObject);
    procedure RadioCutomFileNameStateChanged(Sender: TObject);
  private
    FProcessId : Cardinal;

    {@M}
    procedure DoResize();
    function GetMiniDumpTypesValue() : DWORD;
    procedure SetProcessId(const AValue : Cardinal);
    procedure SetProcessName(const AValue : String);
  public
    {@S}
    property ProcessId  : Cardinal write SetProcessId;
    property ProcessName : String  write SetProcessName;
  end;

var
  ControlFormDumpProcess: TControlFormDumpProcess;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  OptixCore.WinApiEx, OptixCore.Commands.Process;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormDumpProcess.SetProcessId(const AValue : Cardinal);
begin
  FProcessId := AValue;

  labelProcessId.Caption := Format('%d (0x%x)', [FProcessId, FProcessId]);
end;

procedure TControlFormDumpProcess.SetProcessName(const AValue : String);
begin
  LabelProcessName.Caption := AValue;
end;

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

procedure TControlFormDumpProcess.RadioCutomFileNameStateChanged(Sender: TObject);
begin
  EditCustomFilePath.Enabled := RadioCutomFileName.Checked;
end;

procedure TControlFormDumpProcess.RadioTempFileStateChanged(Sender: TObject);
begin
  RadioCutomFileNameStateChanged(Sender);
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

  SendCommand(TOptixCommandDumpProcess.Create(FProcessId, ADestFilePath, ATypesValue));

  Close();
end;

procedure TControlFormDumpProcess.DoResize();
begin
  LabelProcessName.Left := Image.left + Image.Width + ScaleValue(16);
  LabelProcessId.Left   := LabelProcessName.Left;

  LabelProcessName.Top := (PanelHeaderInfo.Height div 2) - (LabelProcessName.Height + ScaleValue(4));
  LabelProcessId.Top   := LabelProcessName.Top + LabelProcessName.Height + ScaleValue(4);

  ButtonCancel.Top := GroupBoxOutputPath.Top + GroupBoxOutputPath.Height + ScaleValue(8);
  ButtonValidate.Top := ButtonCancel.Top;
  ButtonCancel.Left := (ClientWidth div 2) - ButtonCancel.Width - ScaleValue(4);
  ButtonValidate.Left := (ClientWidth div 2) + ScaleValue(4);

  ClientHeight := ButtonCancel.Top + ButtonCancel.Height + PanelMain.Top + ScaleValue(8);
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
