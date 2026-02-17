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



unit uControlFormRemoteShell;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.Types, System.Actions, Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ActnList,

  VirtualTrees.AncestorVCL, VirtualTrees,

  __uBaseFormControl__, uFrameRemoteShellInstance,

  OptixCore.Protocol.Packet, NeoFlat.Panel, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree,

  NeoFlat.PopupMenu, NeoFlat.Button, NeoFlat.ComboBox, OMultiPanel, NeoFlat.ImageButton;
// ---------------------------------------------------------------------------------------------------------------------

type
  TShellInstance = class
  private
    FName          : String;
    FHasUnseenData : Boolean;
    FActive        : Boolean;
    FPanel         : TPanel;
    FFrame         : TFrameRemoteShellInstance;

    {@M}
    function GetInstanceId() : TGUID;
    function GetVisible() : Boolean;
    procedure SetVisible(const AValue : Boolean);
  public
    {@C}
    constructor Create(const AName : String; const APanel : TPanel; const AFrame : TFrameRemoteShellInstance);
    destructor Destroy(); override;

    {@M}
    procedure AddOutput(const AOutput : String);
    procedure SaveToFile(const ADestinationFile : String);
    procedure Close();

    {@G}
    property Id            : TGUID   read GetInstanceId;
    property HasUnseenData : Boolean read FHasUnseenData;
    property Active        : Boolean read FActive;

    {@G/S}
    property Name    : String  read FName      write FName;
    property Visible : Boolean read GetVisible write SetVisible;
  end;

  TControlFormRemoteShell = class(TBaseFormControl)
    ActionList: TActionList;
    NewShellInstance1: TAction;
    BreakActiveShellInstance1: TAction;
    PanelMain: TFlatPanel;
    OMultiPanel: TOMultiPanel;
    PanelInstances: TFlatPanel;
    PanelActions: TFlatPanel;
    ButtonUploadFileToCwd: TFlatImageButton;
    ButtonRenameInstance: TFlatImageButton;
    ButtonBreakInstance: TFlatImageButton;
    ButtonCloseInstance: TFlatImageButton;
    ButtonDeleteInstance: TFlatImageButton;
    ButtonNewInstance: TFlatImageButton;
    PanelComboInstance: TFlatPanel;
    ComboInstance: TFlatComboBox;
    ButtonSaveOutput: TFlatImageButton;
    SD: TSaveDialog;
    SaveInstanceOutputToFile1: TAction;
    procedure NewShellInstance1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HideInstancesList1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonNewInstanceClick(Sender: TObject);
    procedure ButtonCloseInstanceClick(Sender: TObject);
    procedure ButtonDeleteInstanceClick(Sender: TObject);
    procedure ButtonBreakInstanceClick(Sender: TObject);
    procedure ButtonRenameInstanceClick(Sender: TObject);
    procedure BreakActiveShellInstance1Execute(Sender: TObject);
    procedure ComboInstanceChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSaveOutputClick(Sender: TObject);
    procedure SaveInstanceOutputToFile1Execute(Sender: TObject);
  private
    FInstances : TObjectList<TShellInstance>;

    {@M}
    procedure OnInstanceListNotify(Sender: TObject; const AInstance: TShellInstance; Action: TCollectionNotification);
    procedure RequestNewShellInstance();
    function StartShellInstance(const AInstanceId : TGUID) : TShellInstance;
    procedure ShowInstance(const AInstance : TShellInstance);
    procedure CloseShellInstance(const AInstanceId : TGUID);
    function GetShellInstanceById(const AInstanceId : TGUID) : TShellInstance;
    function GetShellInstanceByName(const AName : String) : TShellInstance;
    function ShellNameExists(const AName : String) : Boolean;
    procedure RequestShellInstanceTermination(const AInstance : TShellInstance);
    function GetActiveShellInstance() : TShellInstance;
    procedure RefreshActionButtons();
  protected
    {@M}
    procedure OnFirstShow(); override;
  public
    {@M}
    procedure ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean); override;
  end;

var
  ControlFormRemoteShell: TControlFormRemoteShell;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  OptixCore.Commands, OptixCore.Commands.Shell, Optix.Constants, Optix.Helper, OptixCore.System.FileSystem;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

(* TShellInstance *)

constructor TShellInstance.Create(const AName : String; const APanel : TPanel; const AFrame : TFrameRemoteShellInstance);
begin
  inherited Create();
  ///

  FName          := AName;
  FPanel         := APanel;
  FFrame         := AFrame;
  FActive        := True;
  FHasUnseenData := False;
end;

destructor TShellInstance.Destroy();
begin
  if Assigned(FFrame) then
    FreeAndNil(FFrame);

  if Assigned(FPanel) then
    FreeAndNil(FPanel);

  ///
  inherited Destroy();
end;

procedure TShellInstance.AddOutput(const AOutput : String);
begin
  if Assigned(FFrame) then
    FFrame.AddOutput(AOutput);
end;

procedure TShellInstance.SaveToFile(const ADestinationFile : String);
begin
  if Assigned(FFrame) then
    FFrame.Shell.Lines.SaveToFile(ADestinationFile);
end;

procedure TShellInstance.Close();
begin
  FActive := False;
  if Assigned(FFrame) then
    FFrame.Close();
end;

function TShellInstance.GetInstanceId() : TGUID;
begin
  result := TGUID.Empty;
  ///

  if Assigned(FFrame) then
    result := FFrame.InstanceId;
end;

procedure TShellInstance.SetVisible(const AValue : Boolean);
begin
  if Assigned(FPanel) then
    FPanel.Visible := AValue;
end;

function TShellInstance.GetVisible() : Boolean;
begin
  result := False;
  if Assigned(FPanel) then
    result := FPanel.Visible;
end;

(* TControlFormRemoteShell *)

procedure TControlFormRemoteShell.OnInstanceListNotify(Sender: TObject; const AInstance: TShellInstance;
  Action: TCollectionNotification);
begin
  if not Assigned(AInstance) and Assigned(ComboInstance) then
    Exit();
  ///

  case Action of
    cnAdded: begin
      ComboInstance.Items.Add(AInstance.Name);
    end;

    cnRemoved: begin
      var AIndex := ComboInstance.Items.IndexOf(AInstance.Name);
      if AIndex >= 0 then
        ComboInstance.Items.Delete(AIndex);
    end;
  end;

  ///
  ComboInstance.Refresh();
  RefreshActionButtons();
end;

procedure TControlFormRemoteShell.RefreshActionButtons();
begin
  var AActiveInstance := GetActiveShellInstance();
  ///

  ButtonDeleteInstance.Enabled := Assigned(AActiveInstance);
  ButtonCloseInstance.Enabled  := Assigned(AActiveInstance) and AActiveInstance.Active;
  ButtonBreakInstance.Enabled  := ButtonCloseInstance.Enabled;
  ButtonSaveOutput.Enabled     := ButtonDeleteInstance.Enabled;
  ButtonRenameInstance.Enabled := ButtonDeleteInstance.Enabled;
end;

function TControlFormRemoteShell.GetActiveShellInstance() : TShellInstance;
begin
  result := nil;
  ///

  if not Assigned(FInstances) then
    Exit();

  for var AInstance in FInstances do
    if AInstance.Visible then
      Exit(AInstance);
end;

function TControlFormRemoteShell.GetShellInstanceById(const AInstanceId : TGUID) : TShellInstance;
begin
  result := nil;

  if not Assigned(FInstances) then
    Exit();

  for var AInstance in FInstances do
    if AInstance.Id = AInstanceId then
      Exit(AInstance);
end;

procedure TControlFormRemoteShell.OnFirstShow();
begin
  inherited;

  ///
  RequestNewShellInstance();
end;

procedure TControlFormRemoteShell.NewShellInstance1Execute(Sender: TObject);
begin
  ButtonNewInstanceClick(ButtonNewInstance);
end;

procedure TControlFormRemoteShell.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  inherited;
  ///

  // -------------------------------------------------------------------------------------------------------------------
  if AOptixPacket is TOptixCommandReadShellInstance then begin
    var AInstance := GetShellInstanceById(TOptixCommandReadShellInstance(AOptixPacket).InstanceId);
    if not Assigned(AInstance) then
      AInstance := StartShellInstance(TOptixCommandReadShellInstance(AOptixPacket).InstanceId);

    // TODO:! Unseend data

    ///
    if Assigned(AInstance) then
      AInstance.AddOutput(TOptixCommandReadShellInstance(AOptixPacket).Output);
  end
  // -------------------------------------------------------------------------------------------------------------------
  else if AOptixPacket is TOptixCommandDeleteShellInstance then
    CloseShellInstance(TOptixCommandDeleteShellInstance(AOptixPacket).InstanceId);
  // -------------------------------------------------------------------------------------------------------------------
end;

procedure TControlFormRemoteShell.RequestNewShellInstance();
begin
  SendCommand(TOptixCommandCreateShellInstance.Create());
end;

function TControlFormRemoteShell.GetShellInstanceByName(const AName : String) : TShellInstance;
begin
  result := nil;
  ///

  if not Assigned(FInstances) then
    Exit();

  for var AInstance in FInstances do
    if String.Compare(AInstance.Name, AName, True) = 0 then
      Exit(AInstance);
end;

procedure TControlFormRemoteShell.SaveInstanceOutputToFile1Execute(Sender: TObject);
begin
  ButtonSaveOutputClick(ButtonSaveOutput);
end;

function TControlFormRemoteShell.ShellNameExists(const AName : String) : Boolean;
begin
  result := GetShellInstanceByName(AName) <> nil;
end;

procedure TControlFormRemoteShell.HideInstancesList1Click(Sender: TObject);
begin
  OMultiPanel.PanelCollection.Items[1].Visible := not TMenuItem(Sender).Checked;
end;

procedure TControlFormRemoteShell.ShowInstance(const AInstance : TShellInstance);
begin
  if not Assigned(AInstance) then
    Exit();
  ///

  for var AInstanceCandidate in FInstances do
    AInstanceCandidate.Visible := AInstance = AInstanceCandidate;

  ///
  ComboInstance.ItemIndex := ComboInstance.Items.IndexOf(AInstance.Name);
  RefreshActionButtons();
end;

function TControlFormRemoteShell.StartShellInstance(const AInstanceId : TGUID) : TShellInstance;

  function GenerateRandomShellName() : String;
  begin
    var I := 0;
    while True do begin
      Inc(I);
      ///

      var ACandidate := Format('Session #%d', [I]);
      if not ShellNameExists(ACandidate) then
        Exit(ACandidate);
    end;
  end;

begin
  var AShellName := GenerateRandomShellName();
  var APanel : TPanel := nil;
  var AFrame : TFrameRemoteShellInstance := nil;
  try
    APanel            := TPanel.Create(PanelInstances);
    APanel.Parent     := PanelInstances;
    APanel.Align      := alClient;
    APanel.BevelOuter := bvNone;

    AFrame        := TFrameRemoteShellInstance.Create(APanel, self, AInstanceId);
    AFrame.Parent := APanel;
    AFrame.Align  := alClient;
  except
    if Assigned(AFrame) then
      FreeAndNil(AFrame);

    if Assigned(APanel) then
      FreeAndNil(APanel);
  end;

  result := TShellInstance.Create(AShellName, APanel, AFrame);
  FInstances.Add(result);

  ShowInstance(result);

  // Hacky method to fix annoying issue with Delphi HDPI designing...
  AFrame.Shell.Font.Size := 9;
  AFrame.EditCommand.Font.Size := AFrame.Shell.Font.Size;
  AFrame.EditCommand.SetFocus();
end;

procedure TControlFormRemoteShell.RequestShellInstanceTermination(const AInstance : TShellInstance);
begin
  if not Assigned(AInstance) then
    Exit();
  ///

  SendCommand(TOptixCommandDeleteShellInstance.Create(AInstance.Id));
end;

procedure TControlFormRemoteShell.BreakActiveShellInstance1Execute(Sender: TObject);
begin
  ButtonBreakInstanceClick(ButtonBreakInstance);
end;

procedure TControlFormRemoteShell.ButtonBreakInstanceClick(Sender: TObject);
begin
  var AInstance := GetActiveShellInstance();
  if not Assigned(AInstance) or not AInstance.Active then
    Exit();
  ///

  SendCommand(TOptixCommandSigIntShellInstance.Create(AInstance.Id));
end;

procedure TControlFormRemoteShell.ButtonCloseInstanceClick(Sender: TObject);
begin
  RequestShellInstanceTermination(GetActiveShellInstance());
end;

procedure TControlFormRemoteShell.ButtonDeleteInstanceClick(Sender: TObject);
begin
  var AInstance := GetActiveShellInstance();
  if not Assigned(AInstance) then
    Exit();
  ///

  if Application.MessageBox(
    'This action will close the current remote shell instance and permanently delete all received content. You will ' +
    'lose all captured data from this session.', 'Delete Current Shell Instance',
    MB_ICONQUESTION + MB_YESNO) = ID_NO then
      Exit();

  SendCommand(TOptixCommandDeleteShellInstance.Create(AInstance.Id));

  ///
  FInstances.Remove(AInstance);

  if FInstances.Count > 0 then
    ShowInstance(FInstances.Items[FInstances.Count -1]);
end;

procedure TControlFormRemoteShell.ButtonNewInstanceClick(Sender: TObject);
begin
  RequestNewShellInstance();
end;

procedure TControlFormRemoteShell.ButtonRenameInstanceClick(Sender: TObject);
begin
  var AInstance := GetActiveShellInstance();
  if not Assigned(AInstance) then
    Exit();
  ///

  var AOldName := AInstance.Name;
  var AName := AOldName;
  if not InputQuery('Rename Shell Instance', 'Enter new name', AName) then
    Exit();

  AName := TFileSystemHelper.CleanFileName(AName.Trim());
  if String.Compare(AName, AOldName, True) = 0 then
    Exit();

  if ShellNameExists(AName) then
    Application.MessageBox(
      'The shell instance name already exists. The name must be unique."', 'Rename Shell Instance', MB_ICONERROR);

  AInstance.Name := AName;

  var AIndex := ComboInstance.Items.IndexOf(AOldName);
  if AIndex >= 0 then begin
    ComboInstance.Items.Strings[AIndex] := AName;
    ComboInstance.ItemIndex := AIndex;
  end;
end;

procedure TControlFormRemoteShell.ButtonSaveOutputClick(Sender: TObject);
begin
  var AInstance := GetActiveShellInstance();
  if not Assigned(AInstance) then
    Exit();
  ///

  SD.FileName := AInstance.name;

  if not SD.Execute(self.Handle) then
    Exit();
  ///

  AInstance.SaveToFile(SD.FileName);
end;

procedure TControlFormRemoteShell.CloseShellInstance(const AInstanceId : TGUID);
begin
  var AInstance := GetShellInstanceById(AInstanceId);
  if not Assigned(AInstance) then
    Exit();
  ///

  AInstance.Close();

  ///
  RefreshActionButtons();
end;

procedure TControlFormRemoteShell.ComboInstanceChange(Sender: TObject);
begin
  if TFlatComboBox(Sender).ItemIndex < 0 then
    Exit();
  ///

  var AInstance := GetShellInstanceByName(TFlatComboBox(Sender).Text);

  ShowInstance(AInstance);
end;

procedure TControlFormRemoteShell.FormCreate(Sender: TObject);
begin
  FInstances := TObjectList<TShellInstance>.Create(True);
  FInstances.OnNotify := OnInstanceListNotify;
end;

procedure TControlFormRemoteShell.FormDestroy(Sender: TObject);
begin
  if Assigned(FInstances) then
    FreeAndNil(FInstances);
end;

procedure TControlFormRemoteShell.FormShow(Sender: TObject);
begin
  RefreshActionButtons();
end;

end.
