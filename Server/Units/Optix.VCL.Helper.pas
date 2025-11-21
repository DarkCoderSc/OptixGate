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


unit Optix.VCL.Helper;

interface

uses VCL.Menus, VirtualTrees, VirtualTrees.Types, VCL.Forms, VirtualTrees.BaseTree, Generics.Collections;

type
  TOptixVCLHelper = class
  public
    { TPopupMenu }
    class procedure UpdatePopupMenuRootItemsVisibility(const APopupMenu : TPopupMenu; const AVisible : Boolean); static;
    class procedure HideAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
    class procedure ShowAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
    class procedure ShowForm(const AForm : TForm); static;
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

implementation

uses Winapi.Windows, Winapi.Messages, System.SysUtils;

(* TOptixVirtualTreesFolderTreeHelper *)

{ TOptixVirtualTreesFolderTreeHelper.UpdateTree }
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

{ TOptixVirtualTreesHelper.GetVisibleNodesCount }
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

{ TOptixVirtualTreesHelper.GetColumnIndexByName }
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

{ TOptixVirtualTreesHelper.UpdateColumnVisibility }
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

{ TOptixVirtualTreesHelper.GetRootParentNode }
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

{ TOptixVirtualTreesHelper.SelectNode }
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

(* TOptixVCLHelper *)

{ TOptixVCLHelper.UpdatePopupMenuRootItemsVisibility }
class procedure TOptixVCLHelper.UpdatePopupMenuRootItemsVisibility(const APopupMenu : TPopupMenu; const AVisible : Boolean);
begin
  if not Assigned(APopupMenu) then
    Exit();
  ///

  for var I := 0 to APopupMenu.Items.Count -1 do
    APopupMenu.Items[I].Visible := AVisible;
end;

{ TOptixVCLHelper.HideAllPopupMenuRootItems }
class procedure TOptixVCLHelper.HideAllPopupMenuRootItems(const APopupMenu : TPopupMenu);
begin
  UpdatePopupMenuRootItemsVisibility(APopupMenu, False);
end;

{ TOptixVCLHelper.ShowAllPopupMenuRootItems }
class procedure TOptixVCLHelper.ShowAllPopupMenuRootItems(const APopupMenu : TPopupMenu);
begin
  UpdatePopupMenuRootItemsVisibility(APopupMenu, True);
end;

{ TOptixVCLHelper.ShowForm }
class procedure TOptixVCLHelper.ShowForm(const AForm : TForm);
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

end.
