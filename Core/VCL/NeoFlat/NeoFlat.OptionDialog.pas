{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                              Project: Optix Neo                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter & wife                                }
{                                                                              }
{******************************************************************************}

unit NeoFlat.OptionDialog;

interface

uses System.Classes, VCL.Forms, NeoFlat.SettingHandler;

type
  TFlatOptionDialog = class;

  TOptionKind = (
    okCheckBox
  );

  TFlatOptionItem = class(TCollectionItem)
  private
    FKind         : TOptionKind;
    FCaption      : String;
    FChecked      : Boolean;
    FHint         : String;
    FEnabled      : Boolean;
    FName         : String;

    FOptionDialog : TFlatOptionDialog;
  public
    {@C}
    constructor Create(ACollection : TCollection); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Kind    : TOptionKind read FKind    write FKind;
    property Caption : String      read FCaption write FCaption;
    property Checked : Boolean     read FChecked write FChecked;
    property Hint    : String      read FHint    write FHint;
    property Enabled : Boolean     read FEnabled write FEnabled;
    property Name    : String      read FName    write FName;
  end;

  TFlatOptionItems = class(TOwnedCollection)
  private
  protected
    {@M}
    function GetItem(Index: Integer): TFlatOptionItem;
    procedure SetItem(Index: Integer; Value: TFlatOptionItem);
    procedure Notify(Item : TCollectionItem; Action : TCollectionNotification); override;
  public
    {@C}
    constructor Create(AOwner: TPersistent);

    {@M}
    function Add: TFlatOptionItem;
    function Get(AName : String; ADefault : Boolean) : Boolean;

    {@G/S}
    property Items[Index: Integer]: TFlatOptionItem read GetItem write SetItem; default;
  end;

  TFlatOptionDialog = class(TComponent)
  private
    FOptions     : TFlatOptionItems;
    FCaption     : String;
    FWidth       : Integer;
    FSetting     : TFlatSettingHandler;
    FParentChain : String;

    {@M}
    procedure LoadFromSetting();
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure Show(AOwnerForm : TForm);
    function Get(AName : String; ADefault : Boolean) : Boolean;
    procedure Loaded(); override;
    procedure BeforeDestruction(); override;
  published
    {@G}
    property Options  : TFlatOptionItems    read FOptions  write FOptions;
    property Caption  : String            read FCaption  write FCaption;
    property Width    : Integer           read FWidth    write FWidth;
    property Setting  : TFlatSettingHandler read FSetting  write FSetting;
  end;

implementation

uses System.SysUtils, NeoFlat.OptionDialogForm, Winapi.Windows, NeoFlat.Common;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TFlatOptionDialog


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{ TFlatOptionDialog.Loaded }

procedure TFlatOptionDialog.Loaded();
begin
  inherited Loaded();
  ///

  FParentChain := GetParentChain(self);

  if Assigned(FSetting) then
    self.LoadFromSetting();
end;

{ TFlatOptionDialog.BeforeDestruction }

procedure TFlatOptionDialog.BeforeDestruction();
var AItem : TFlatOptionItem;
    I     : Integer;
begin
  if not Assigned(FSetting) then
    Exit();

  for I := 0 to FOptions.Count -1 do begin
    AItem := FOptions.Items[i];

    FSetting.Write(AItem.Name, AItem.Checked, FParentChain);
  end;

  ///
  inherited BeforeDestruction();
end;

{ TFlatOptionDialog.LoadFromSetting }

procedure TFlatOptionDialog.LoadFromSetting();
var AItem : TFlatOptionItem;
    I     : Integer;
begin
  if not Assigned(FSetting) then
    Exit();

  for I := 0 to FOptions.Count -1 do begin
    AItem := FOptions.Items[i];

    AItem.Checked := FSetting.Read(AItem.Name, AItem.Checked, FParentChain);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TFlatOptionDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOptions     := TFlatOptionItems.Create(self);
  FCaption     := 'Options';
  FSetting     := nil;
  FWidth       := 215;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TFlatOptionDialog.Destroy();
begin
  if Assigned(FOptions) then
    FreeAndNil(FOptions);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Show option form (if Refresh is set to true, we re render all options)
-------------------------------------------------------------------------------}
procedure TFlatOptionDialog.Show(AOwnerForm : TForm);
var AOptionForm : TSub7FormOptionDialog;
begin
  AOptionForm := TSub7FormOptionDialog.Create(AOwnerForm, self);
  try
    AOptionForm.Render();

    ///
    AOptionForm.ShowModal();
  finally
    if Assigned(AOptionForm) then
      FreeAndNil(AOptionForm);
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve options checked status
-------------------------------------------------------------------------------}
function TFlatOptionDialog.Get(AName : String; ADefault : Boolean) : Boolean;
begin
  result := self.Options.Get(AName, ADefault);
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TFlatOptionItem


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor TFlatOptionItem.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  ///

  FKind         := okCheckBox;
  FCaption      := Format('Option n°%d', [self.Index]);
  FName         := Format('Option%d', [self.Index]);
  FChecked      := False;
  FHint         := '';
  FEnabled      := True;

  FOptionDialog := TFlatOptionDialog(TFlatOptionItems(self.GetOwner).Owner);
end;

destructor TFlatOptionItem.Destroy();
begin
  inherited Destroy();
  ///

end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TFlatOptionItems


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

function TFlatOptionItems.Add: TFlatOptionItem;
begin
  Result := TFlatOptionItem(inherited Add);
end;

function TFlatOptionItems.Get(AName : String; ADefault : Boolean) : Boolean;
var I : integer;
begin
  result := ADefault;
  ///

  for i := 0 to self.Count -1 do begin
    if String.Compare(self.Items[i].Name, AName, True) = 0 then begin
      result := self.Items[i].Checked;

      break;
    end;
  end;
end;

constructor TFlatOptionItems.create(AOwner : TPersistent);
begin
  inherited Create(AOwner, TFlatOptionItem);
  ///

end;

function TFlatOptionItems.GetItem(Index: Integer): TFlatOptionItem;
begin
  Result := TFlatOptionItem(inherited GetItem(Index));
end;

procedure TFlatOptionItems.SetItem(Index: Integer; Value: TFlatOptionItem);
begin
  inherited SetItem(Index, Value);
  ///

end;

{ TFlatOptionItems.Notify }

procedure TFlatOptionItems.Notify(Item : TCollectionItem; Action : TCollectionNotification);
begin
  inherited Notify(Item, Action);
  ///

  case Action of
    cnAdding: ;
    cnAdded: ;
    cnExtracting: ;
    cnExtracted: ;
    cnDeleting: ;
    cnRemoved: ;
  end;
end;

end.
