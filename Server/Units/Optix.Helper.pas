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



unit Optix.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  Winapi.ShellAPI,

  VCL.Menus, VCL.Forms, VCL.Controls,

  VirtualTrees, VirtualTrees.BaseTree, VirtualTrees.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixHelper = class
  public
    { TPopupMenu Helpers }
    class procedure UpdatePopupMenuRootItemsVisibility(const APopupMenu : TPopupMenu; const AVisible : Boolean); static;
    class procedure HideAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
    class procedure ShowAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
    class procedure ShowForm(const AForm : TForm); static;

    { Text / Format Helpers }
    class function Pluralize(const AText, ASuffix : String; const ACount : Cardinal;
      APluralForm : String = '') : String; static;
    class function FormatInt(const AInteger : Integer) : String; static;
    class function DefaultIfEmpty(const AValue : String; const ADefault : String = '-') : String;
    class function FormatFileSize(const ASize : Int64) : string;
    class procedure CheckCertificateFingerprint(const AValue : String);

    { Dialogs / Forms Helpers }
    class function Error(const AHandle : THandle; const AMessage : String) : Integer; static;

    { Date Helpers }
    class function ElapsedTime(const ADays, AHours, AMinutes, ASeconds : UInt64) : String; overload; static;
    class function ElapsedTime(const AMilliseconds : UInt64) : String; overload; static;
    class function ElapsedDateTime(const AFirstDateTime, ASecondDateTime : TDateTime) : String; static;

    { System / Windows / Shell Helpers }
    class function ReadResourceString(const AResourceName : String) : String; static;
    class function TryReadResourceString(const AResourceName : String) : String; static;
    class procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo;
      const ALargeIcon : Boolean = False); static;
    class function SystemFileIcon(const AFileName : string; AExtensionMode : Boolean = False) : Integer; static;
    class function SystemFolderIcon(APath : String = '') : Integer; static;
    class function GetWindowsDirectory() : string; static;
    class procedure Open(const ACommand : String); static;

    { Data Comparison Helpers }
    class function CompareObjectAssignement(const AObject1, AObject2 : TObject) : Integer; static;
    class function ComparePointerAssigmenet(const pPtr1, pPtr2 : Pointer) : Integer; static;
    class function CompareDateTimeEx(const ADate1 : TDateTime; const ADate1IsSet : Boolean; const ADate2 : TDateTime;
      const ADate2IsSet : Boolean) : Integer; static;
  end;

  TOptixVirtualTreesHelper = class
  public
    class function GetVisibleNodesCount(const AVST : TVirtualStringTree) : UInt64; static;
    class function GetColumnIndexByName(const AVST : TVirtualStringTree; const AName : String) : Integer; static;
    class procedure UpdateColumnVisibility(const AVST : TVirtualStringTree; const AName : String;
      AVisible : Boolean); static;
    class function GetRootParentNode(const AVST : TVirtualStringTree;
      const pNode : PVirtualNode) : PVirtualNode; static;
    class procedure SelectNode(const AVST : TVirtualStringTree; const pNode : PVirtualNode); static;
  end;

  TOptixVirtualTreesFolderTreeHelper = class
  type
    TCompareFolderNameCallback = reference to function(const pData : Pointer; const ACompareTo : String) : Boolean;
    TGetFolderNameFromItemCallback<T> = reference to function(const AItem: T): String;
    TGetFolderNameFromDataCallback = reference to function(const pData: Pointer): String;
    TSetupNodeDataCallback<T> = reference to procedure(var pNode, pParentNode : PVirtualNode; const AItem : T);
  public
    class procedure UpdateTree<T>(const AVST : TVirtualStringTree; const AParentsItems: TEnumerable<T>;
      ALevelItems: TEnumerable<T>; const AGetNameFromDataFunc : TGetFolderNameFromDataCallback;
      const AGetNameFromItemFunc : TGetFolderNameFromItemCallback<T>;
      const ASetupNodeDataFunc : TSetupNodeDataCallback<T>); static;
  end;

  TOptixErrorDialog = class
  private
    FOwnerForm : TForm;
    FErrors    : TStringList;
  public
    {@C}
    constructor Create(const AOwnerForm : TForm);
    destructor Destroy(); override;

    {@M}
    procedure Add(const AError : String);
    function ShowErrors() : Boolean;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.DateUtils, System.RegularExpressions, System.IOUtils, System.Math, System.TimeSpan,

  Winapi.Windows, Winapi.Messages;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixVirtualTreesFolderTreeHelper *)

class procedure TOptixVirtualTreesFolderTreeHelper.UpdateTree<T>(const AVST : TVirtualStringTree;
  const AParentsItems: TEnumerable<T>; ALevelItems: TEnumerable<T>;
  const AGetNameFromDataFunc : TGetFolderNameFromDataCallback;
  const AGetNameFromItemFunc : TGetFolderNameFromItemCallback<T>;
  const ASetupNodeDataFunc : TSetupNodeDataCallback<T>);
begin
  if not Assigned(AVST) then
    Exit();
  ///

  var pParentNode := PVirtualNode(nil);
  var pChildNode := PVirtualNode(nil);
  var pNode := PVirtualNode(nil);

  AVST.BeginUpdate();
  try
    // Generate or Update Parent Folder Tree
    if Assigned(AParentsItems) then begin
      for var AItem in AParentsItems do begin
        pChildNode := nil;

        pNode := AVST.GetFirstChild(pParentNode);
        while Assigned(pNode) do begin
          if SameText(AGetNameFromDataFunc(pNode.GetData), AGetNameFromitemFunc(AItem)) then begin
            pChildNode := pNode;

            break;
          end;

          ///
          pNode := AVST.GetNextSibling(pNode);
        end;

        ///
        ASetupNodeDataFunc(pChildNode, pParentNode, AItem);

        pParentNode := pChildNode;
      end;
    end;

    if Assigned(ALevelItems) then begin
      // Generate Or Update Level Folder
      var ALevelNodesCache := TDictionary<String, PVirtualNode>.Create(); // For Optimization
      var ALevelItemsCache := TList<String>.Create();
      try
        // Fill Cache with Existing Nodes
        pNode := AVST.GetFirstChild(pParentNode);
        while Assigned(pNode) do begin
          if pNode.GetData <> nil then
            ALevelNodesCache.Add(AGetNameFromDataFunc(pNode.GetData), pNode);

          ///
          pNode := AVST.GetNextSibling(pNode);
        end;

        // Insert or Update Nodes
        var AItemName : String;
        var ANewNode : Boolean;

        for var AItem in ALevelItems do begin
          pNode := nil;
          pChildNode := AVST.GetFirstChild(pParentNode);
          ///

          AItemName := AGetNameFromItemFunc(AItem);

          ALevelItemsCache.Add(AItemName);

          ANewNode := not ALevelNodesCache.TryGetValue(AItemName, pNode);

          ///
          ASetupNodeDataFunc(pNode, pParentNode, AItem);

          if ANewNode then
            ALevelNodesCache.Add(AItemName, pNode);
        end;

        // Delete Missing Nodes
        for var APair in ALevelNodesCache do
          if not ALevelItemsCache.Contains(APair.Key) then
            AVST.DeleteNode(APair.Value);
      finally
        if Assigned(ALevelItemsCache) then
          FreeAndNil(ALevelItemsCache);

        if Assigned(ALevelNodesCache) then
          FreeAndNil(ALevelNodesCache);
      end;
    end;
  finally
    if Assigned(pParentNode) then begin
      AVST.Expanded[pParentNode] := True;

      TOptixVirtualTreesHelper.SelectNode(AVST, pParentNode);
    end;

    AVST.SortTree(0, TSortDirection.sdAscending);

    AVST.EndUpdate();
  end;
end;

(* TOptixVirtualTreesHelper *)

class function TOptixVirtualTreesHelper.GetVisibleNodesCount(const AVST : TVirtualStringTree) : UInt64;
begin
  result := 0;
  ///

  if not Assigned(AVST) then
    Exit();

  for var pNode in AVST.Nodes do begin
    if vsVisible in pNode.States then
      Inc(result);
  end;
end;

class function TOptixVirtualTreesHelper.GetColumnIndexByName(const AVST : TVirtualStringTree; const AName : String) : Integer;
begin
  result := -1;
  ///

  if not Assigned(AVST) then
    Exit();

  for var AIndex := 0 to AVST.Header.Columns.Count -1 do begin
    if String.Compare(AName, AVST.Header.Columns.Items[AIndex].Text, True) = 0 then begin
      result := AIndex;

      ///
      break;
    end;
  end;
end;

class procedure TOptixVirtualTreesHelper.UpdateColumnVisibility(const AVST : TVirtualStringTree; const AName : String; AVisible : Boolean);
begin
  var AColumnIndex := GetColumnIndexByName(AVST, AName);
  if AColumnIndex = -1 then
    Exit();
  ///

  var AColumn := AVST.Header.Columns.Items[AColumnIndex];

  if AVisible then
    AColumn.Options := AColumn.Options + [coVisible]
  else
    AColumn.Options := AColumn.Options - [coVisible];
end;

class function TOptixVirtualTreesHelper.GetRootParentNode(const AVST : TVirtualStringTree;
  const pNode : PVirtualNode) : PVirtualNode;
begin
  result := nil;
  ///

  if not Assigned(pNode) or not Assigned(AVST) then
    Exit();

  result := pNode;

  while Assigned(AVST.NodeParent[result]) do
    result := AVST.NodeParent[result];
end;

class procedure TOptixVirtualTreesHelper.SelectNode(const AVST : TVirtualStringTree; const pNode : PVirtualNode);
begin
  if not Assigned(AVST) or not Assigned(pNode) then
    Exit();
  ///

  AVST.ClearSelection();
  ///

  AVST.FocusedNode := pNode;
  AVST.Selected[pNode] := True;
  AVST.ScrollIntoView(pNode, True);
end;

(* TOptixHelper *)

class procedure TOptixHelper.UpdatePopupMenuRootItemsVisibility(const APopupMenu : TPopupMenu; const AVisible : Boolean);
begin
  if not Assigned(APopupMenu) then
    Exit();
  ///

  for var I := 0 to APopupMenu.Items.Count -1 do
    APopupMenu.Items[I].Visible := AVisible;
end;

class procedure TOptixHelper.HideAllPopupMenuRootItems(const APopupMenu : TPopupMenu);
begin
  UpdatePopupMenuRootItemsVisibility(APopupMenu, False);
end;

class procedure TOptixHelper.ShowAllPopupMenuRootItems(const APopupMenu : TPopupMenu);
begin
  UpdatePopupMenuRootItemsVisibility(APopupMenu, True);
end;

class procedure TOptixHelper.ShowForm(const AForm : TForm);
begin
  if not Assigned(AForm) then
    Exit();
  ///

  if IsIconic(AForm.Handle) then begin
    if Application.MainForm.Handle = AForm.Handle then
      SendMessage(AForm.Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      ShowWindow(AForm.Handle, SW_RESTORE);
  end else
    AForm.Show();
end;

class function TOptixHelper.Pluralize(const AText, ASuffix : String; const ACount : Cardinal;
  APluralForm : String = '') : String;
begin
  if ACount <= 1 then
    result := AText
  else if APluralForm <> '' then
    result := APluralForm
  else
    result := AText + ASuffix;
end;

class function TOptixHelper.FormatInt(const AInteger : Integer) : String;
begin
  result := Format('%d (0x%p)', [
    AInteger,
    Pointer(AInteger)
  ]);
end;

class function TOptixHelper.Error(const AHandle : THandle; const AMessage : String) : Integer;
begin
  result := MessageBoxW(AHandle, PWideChar(AMessage), 'Error', MB_ICONHAND);
end;

class function TOptixHelper.CompareDateTimeEx(const ADate1 : TDateTime; const ADate1IsSet : Boolean; const ADate2 : TDateTime; const ADate2IsSet : Boolean) : Integer;
begin
  if not ADate1IsSet and not ADate2IsSet then
    result := 0
  else if not ADate1IsSet and ADate2IsSet then
    result := 1
  else if ADate1IsSet and not ADate2IsSet then
    result := -1
  else
    result := CompareDateTime(ADate1, ADate2);
end;

class function TOptixHelper.CompareObjectAssignement(const AObject1, AObject2 : TObject) : Integer;
begin
  if not Assigned(AObject1) and not Assigned(AObject2) then
    Result := 0
  else if not Assigned(AObject1) then
    Result := 1
  else
    Result := -1
end;

class function TOptixHelper.ComparePointerAssigmenet(const pPtr1, pPtr2 : Pointer) : Integer;
begin
  if (pPtr1 = nil) and (pPtr2 = nil) then
    Result := 0
  else if pPtr1 <> nil then
    Result := 1
  else
    Result := -1
end;

class procedure TOptixHelper.CheckCertificateFingerprint(const AValue : String);
begin
  if not TRegEx.IsMatch(AValue, '^([0-9A-Fa-f]{2}:){63}[0-9A-Fa-f]{2}$') then
    raise Exception.Create(
      'Invalid certificate fingerprint. It must be a valid SHA-512 fingerprint, with each byte separated by a colon ' +
      '(e.g., AA:BB:CC:DD…:FF).'
    );
end;

class procedure TOptixHelper.Open(const ACommand : String);
begin
  ShellExecute(0, 'open', PWideChar(ACommand), nil, nil, SW_SHOW);
end;

class function TOptixHelper.GetWindowsDirectory() : string;
begin
  SetLength(result, MAX_PATH);

  var ALen := WinAPI.Windows.GetWindowsDirectory(@result[1], MAX_PATH);

  SetLength(result, ALen);
  if ALen > MAX_PATH then
    WinAPI.Windows.GetWindowsDirectory(@result[1], ALen);

  ///
  result := IncludeTrailingPathDelimiter(result);
end;

class procedure TOptixHelper.InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; const ALargeIcon : Boolean = False);
var AFlags : Integer;
begin
  ZeroMemory(@AFileInfo, SizeOf(TSHFileInfo));
  ///

  if ALargeIcon then
    AFlags := SHGFI_LARGEICON
  else
    AFlags := SHGFI_SMALLICON;

  AImages.Handle := SHGetFileInfo(
                                    PChar(TPath.GetPathRoot(GetWindowsDirectory())),
                                    0,
                                    AFileInfo,
                                    SizeOf(AFileInfo),
                                    AFlags or (SHGFI_SYSICONINDEX)
  );
end;

class function TOptixHelper.SystemFileIcon(const AFileName : string; AExtensionMode : Boolean = False) : Integer;
var AFileInfo : TSHFileInfo;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  if not AExtensionMode then
    AExtensionMode := not FileExists(AFileName);

  var AFlags := SHGFI_SMALLICON or SHGFI_SYSICONINDEX;
  if AExtensionMode then
    AFlags := AFlags or SHGFI_USEFILEATTRIBUTES;

  SHGetFileInfo(PWideChar(AFileName), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

class function TOptixHelper.SystemFolderIcon(APath : String = '') : Integer;
var AFileInfo : TSHFileInfo;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  if APath = '' then
    APath := GetWindowsDirectory();

  var AFlags := SHGFI_SYSICONINDEX;

  SHGetFileInfo(PChar(APath), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

class function TOptixHelper.FormatFileSize(const ASize : Int64) : string;
const AByteDescription : array[0..9-1] of string = (
  'Bytes', 'KiB', 'MB', 'GiB', 'TB',
  'PB', 'EB', 'ZB', 'YB'
);
begin
  var ACount := 0;

  while ASize > Power(1024, ACount +1) do
    Inc(ACount);

  ///
  result := Format('%s %s', [
    FormatFloat('###0.00', ASize / Power(1024, ACount)),
    AByteDescription[ACount]]
  );
end;

class function TOptixHelper.ReadResourceString(const AResourceName : String) : String;
begin
  var AResourceStream := TResourceStream.Create(hInstance, AResourceName, RT_RCDATA);
  try
    SetString(result, PAnsiChar(AResourceStream.Memory), AResourceStream.Size);
  finally
    FreeAndNil(AResourceStream);
  end;
end;

class function TOptixHelper.TryReadResourceString(const AResourceName : String) : String;
begin
  try
    result := ReadResourceString(AResourceName);
  except
    result := '';
  end;
end;

class function TOptixHelper.DefaultIfEmpty(const AValue : String; const ADefault : String = '-') : String;
begin
  if String.IsNullOrEmpty(AValue) then
    result := ADefault
  else
    result := AValue;
end;

class function TOptixHelper.ElapsedTime(const ADays, AHours, AMinutes, ASeconds : UInt64) : String;
begin
  if ADays > 0 then
    result := Format('%d days, %.2d:%.2d:%.2d', [
      ADays,
      AHours,
      AMinutes,
      ASeconds
    ])
  else if AHours > 0 then
    result := Format('%.2d:%.2d:%.2d', [
      AHours,
      AMinutes,
      ASeconds
    ])
  else if AMinutes > 0 then
    result := Format('%d minutes and %.2d seconds.', [
      AMinutes,
      ASeconds
    ])
  else if ASeconds >= 0 then
    result := Format('%d seconds ago.', [ASeconds]);
end;

class function TOptixHelper.ElapsedTime(const AMilliseconds : UInt64) : String;
var ASpan : TTimeSpan;
begin
  ASpan := TTimeSpan.FromMilliseconds(AMilliseconds);
  ///

  result := ElapsedTime(
    ASpan.Days,
    ASpan.Hours,
    ASpan.Minutes,
    ASpan.Seconds
  );
end;

class function TOptixHelper.ElapsedDateTime(const AFirstDateTime, ASecondDateTime : TDateTime) : String;
var AElaspedTime  : TDateTime;
    ADays         : Integer;
    AHours        : Word;
    AMinutes      : Word;
    ASeconds      : Word;
    AMilliSeconds : Word;
begin
  AElaspedTime := ASecondDateTime - AFirstDateTime;
  ///

  ADays := DaysBetween(ASecondDateTime, AFirstDateTime);

  DecodeTime(AElaspedTime, AHours, AMinutes, ASeconds, AMilliSeconds);

  ///
  result := ElapsedTime(ADays, AHours, AMinutes, ASeconds);
end;

(* TOptixErrorDialog *)

constructor TOptixErrorDialog.Create(const AOwnerForm : TForm);
begin
  inherited Create();
  ///

  FOwnerForm := AOwnerForm;
  FErrors    := TStringList.Create();
end;

destructor TOptixErrorDialog.Destroy();
begin
  if Assigned(FErrors) then
    FreeAndNil(FErrors);

  ///
  inherited;
end;

procedure TOptixErrorDialog.Add(const AError : String);
begin
  FErrors.Add(AError);
end;

function TOptixErrorDialog.ShowErrors() : Boolean;
begin
  if FErrors.Count = 0 then
    Exit(False);
  ///

  result := True;

  var AMessageBuilder := TStringBuilder.Create();
  try
    AMessageBuilder.AppendLine(
      Format('The following %s occured:', [TOptixHelper.Pluralize('error', 's', FErrors.Count)])
    );
    AMessageBuilder.AppendLine();

    for var AError in FErrors do
      AMessageBuilder.AppendLine(' - ' + AError);

    ///
    MessageBoxW(FOwnerForm.Handle, PWideChar(AMessageBuilder.ToString()), 'Error', MB_ICONHAND);
  finally
    FreeAndNil(AMessageBuilder);
  end;
end;

end.
