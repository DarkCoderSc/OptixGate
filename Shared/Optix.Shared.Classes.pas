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



unit Optix.Shared.Classes;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  XSuperObject,

  Optix.Interfaces;
// ---------------------------------------------------------------------------------------------------------------------

type
  OptixSerializableAttribute = class(TCustomAttribute);

  TOptixSerializableObject = class(TInterfacedPersistent, IOptixSerializable)
  private
    const
      SEARCH_LIST_PATTERN = '(?:.*\.|^)(TObjectList|TList)<\s*(?:.*\.)?([A-Za-z_]\w*)\s*>';
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); virtual;
    procedure AfterCreate(); virtual;
  public
    {@M}
    function Serialize() : ISuperObject; virtual;

    {@C}
    constructor Create(); overload; virtual;
    constructor Create(const ASerializedObject : ISuperObject); overload; virtual;
  end;

  TOptixMemoryObject = class
  private
    FAddress    : Pointer;
    FSize       : UInt64;
    FOwnsMemory : Boolean;

    {@M}
    procedure FreeMemory();
    function GetAsBase64() : String;
    function GetHasData() : Boolean;
  public
    {@C}
    constructor Create(); overload;
    procedure CopyFrom(const pCopyFromAddress : Pointer; const ASize : UInt64); overload;
    procedure CopyFrom(const AOptixMemoryObject : TOptixMemoryObject); overload;
    procedure Assign(const pMemoryAddress : Pointer; const ASize : UInt64);
    constructor Create(const ABase64Data : String); overload;
    destructor Destroy(); override;

    {@G}
    property HasData  : Boolean read GetHasData;
    property Address  : Pointer read FAddress;
    property Size     : UInt64  read FSize;
    property ToBase64 : String  read GetAsBase64;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Winapi.Windows,

  System.NetEncoding, System.Rtti, System.TypInfo, System.SysUtils, System.Hash, System.RegularExpressions;
// ---------------------------------------------------------------------------------------------------------------------

function TOptixSerializableObject.Serialize() : ISuperObject;
begin
  result := SO();
  ///

  var AContext := TRttiContext.Create();
  var AType := AContext.GetType(ClassType);

  for var AField in AType.GetFields() do begin
    for var AFieldAttribute in AField.GetAttributes() do begin
      if not (AFieldAttribute is OptixSerializableAttribute) then
        continue;
      ///

      case AField.FieldType.TypeKind of
        // Handle compatible records -----------------------------------------------------------------------------------
        tkRecord : begin
          // TGUID -----------------------------------------------------------------------------------------------------
          if AField.FieldType.Handle = TypeInfo(TGUID) then
            result.S[AField.Name] := AField.GetValue(self).AsType<TGUID>.ToString();
          // -----------------------------------------------------------------------------------------------------------
        end;

        // Objects -----------------------------------------------------------------------------------------------------
        tkClass : begin
          var AFieldClass := AField.FieldType.AsInstance.MetaclassType;
          var AObject := AField.GetValue(self).AsObject;
          if not Assigned(AObject) then
            continue;
          ///

          // Serializable Object ---------------------------------------------------------------------------------------
          if AFieldClass.InheritsFrom(TOptixSerializableObject) then begin
            var ASerializableObject := AObject as TOptixSerializableObject;

            ///
            result.O[AField.Name] := ASerializableObject.Serialize();
          // Memory Object ---------------------------------------------------------------------------------------------
          end else if AFieldClass.InheritsFrom(TOptixMemoryObject) then
            result.S[AField.Name] := (AObject as TOptixMemoryObject).ToBase64
          else if TRegEx.Match(AFieldClass.ClassName, SEARCH_LIST_PATTERN).Success then begin
            var AToArrayMethod := AField.FieldType.GetMethod('ToArray');
            if not Assigned(AToArrayMethod) then
              Exit();
            ///

            var AInvokedMethod := AToArrayMethod.Invoke(AObject, []);

            var AJsonArray := SA();

            for var I := 0 to AInvokedMethod.GetArrayLength -1 do begin
              var AItemObject := AInvokedMethod.GetArrayElement(I).AsObject();
              ///

              if not Assigned(AItemObject) or not (AItemObject is TOptixSerializableObject) then
                continue;

              ///
              AJsonArray.Add(TOptixSerializableObject(AItemObject).Serialize())
            end;

            ///
            result.A[AField.Name] := AJsonArray;
          end;
          // -----------------------------------------------------------------------------------------------------------
        end;
        // Sets --------------------------------------------------------------------------------------------------------
        tkSet :
          result.S[AField.Name] := SetToString(AField.FieldType.Handle, AField.GetValue(self).GetReferenceToRawData);
        // Enum --------------------------------------------------------------------------------------------------------
        tkEnumeration :
          result.I[AField.Name] := AField.GetValue(self).AsOrdinal();
        // Variants -----------------------------------------------------------------------------------------------------
        tkInteger, tkFloat, tkInt64, tkChar, tkString, tkWChar, tkLString, tkWString, tkUString :
          result.V[AField.Name] := AField.GetValue(self).AsVariant();
        // -------------------------------------------------------------------------------------------------------------
      end;
    end;
  end;
end;

procedure TOptixSerializableObject.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    Exit();
  ///

  var AContext := TRttiContext.Create();
  var AType := AContext.GetType(ClassType);

  for var APair in ASerializedObject.AsObject() do begin
    var AField := AType.GetField(APair.Name);
    if not Assigned(AField) or not Assigned(AField.GetAttribute(OptixSerializableAttribute)) then
      continue;
    ///

    case AField.FieldType.TypeKind of
      // Handle compatible records -------------------------------------------------------------------------------------
      tkRecord : begin
        // TGUID -------------------------------------------------------------------------------------------------------
        if AField.FieldType.Handle = TypeInfo(TGUID) then
          AField.SetValue(self, TValue.From<TGUID>(TGUID.Create(APair.AsString)));
        // -------------------------------------------------------------------------------------------------------------
      end;

      // Objects -------------------------------------------------------------------------------------------------------
      tkClass : begin
        var AFieldClass := AField.FieldType.AsInstance.MetaclassType;
        var AObject := AField.GetValue(self).AsObject;
        ///

        // Serializable Object -----------------------------------------------------------------------------------------
        if AFieldClass.InheritsFrom(TOptixSerializableObject) and Assigned(AObject) then begin
          var ASerializableObject := AObject as TOptixSerializableObject;

          ///
          ASerializableObject.DeSerialize(APair.AsObject);
        // Memory Object -----------------------------------------------------------------------------------------------
        end else if AFieldClass.InheritsFrom(TOptixMemoryObject) then begin
          if Assigned(AObject) then
            FreeAndNil(AObject);
          ///

          AField.SetValue(self, TOptixMemoryObject.Create(APair.AsString));
        end else if TRegEx.Match(AFieldClass.ClassName, SEARCH_LIST_PATTERN).Success then begin
          // TODO: Create object if not already + support OwnsObject
          var AAddMethod := AField.FieldType.GetMethod('Add');
          var AClearMethod := AField.FieldType.GetMethod('Clear');
          ///

          if not Assigned(AClearMethod) or not Assigned(AAddMethod) or (Length(AAddMethod.GetParameters) = 0) then
            Exit();

          AClearMethod.Invoke(AObject, []);

          var AJsonArray := ASerializedObject.A[AField.Name];

          // Trick to get <TObjectKind> class from the list using its first argument ("Add() method")
          var AItemType := AAddMethod.GetParameters[0].ParamType;
          var AItemClass := AItemType.AsInstance.MetaclassType;

          for var I := 0 to AJsonArray.Length -1 do begin
            var AItem := AItemClass.Create();
            if not (AItem is TOptixSerializableObject) then
              break;
            ///

            TOptixSerializableObject(AItem).DeSerialize(AJsonArray.O[I]);

            AAddMethod.Invoke(AObject, [AItem]);
          end;
        end;
        // -------------------------------------------------------------------------------------------------------------
      end;

      // Variants ------------------------------------------------------------------------------------------------------
      tkInteger, tkChar, tkFloat, tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64, tkUString :
        AField.SetValue(self, TValue.FromVariant(APair.AsVariant));

      // Enums ---------------------------------------------------------------------------------------------------------
      tkEnumeration : begin
        var ATypeData := GetTypeData(AField.FieldType.Handle);
        if (APair.AsInteger >= ATypeData^.MinValue) and (APair.AsInteger <= ATypeData^.MaxValue) then
          AField.SetValue(self, TValue.FromOrdinal(AField.FieldType.Handle, APair.AsInteger));
      end;

      // Sets ----------------------------------------------------------------------------------------------------------
      tkSet :
        try
          StringToSet(AField.FieldType.Handle, APair.AsString, Pointer(NativeUInt(self) + NativeUInt(AField.Offset)));
        except
        end;
      // ---------------------------------------------------------------------------------------------------------------
    end;
  end;
end;

procedure TOptixSerializableObject.AfterCreate();
begin
  ///
end;

constructor TOptixSerializableObject.Create();
begin
  inherited;
  ///

  AfterCreate();
end;

constructor TOptixSerializableObject.Create(const ASerializedObject : ISuperObject);
begin
  Create();
  ///

  if Assigned(ASerializedObject) then
    DeSerialize(ASerializedObject);
end;

(* TOptixMemoryObject *)

constructor TOptixMemoryObject.Create();
begin
  inherited;
  ///

  FOwnsMemory := False;

  FreeMemory();
end;

procedure TOptixMemoryObject.FreeMemory();
begin
  if FOwnsMemory and Assigned(FAddress) then
    FreeMem(FAddress, FSize);
  ///

  FAddress := nil;
  FSize := 0;
end;

procedure TOptixMemoryObject.CopyFrom(const pCopyFromAddress : Pointer; const ASize : UInt64);
begin
  FreeMemory();
  FOwnsMemory := True;
  ///

  if ASize = 0 then
    Exit();

  FSize := ASize;

  GetMem(FAddress, FSize);

  CopyMemory(FAddress, pCopyFromAddress, FSize);
end;

procedure TOptixMemoryObject.CopyFrom(const AOptixMemoryObject : TOptixMemoryObject);
begin
  if Assigned(AOptixMemoryObject) then
    CopyFrom(AOptixMemoryObject.Address, AOptixMemoryObject.Size);
end;

procedure TOptixMemoryObject.Assign(const pMemoryAddress : Pointer; const ASize : UInt64);
begin
  FreeMemory();
  FOwnsMemory := False;
  ///

  FAddress := pMemoryAddress;
  FSize := ASize;
end;

function TOptixMemoryObject.GetHasData() : Boolean;
begin
  result := Assigned(FAddress) and (FSize > 0);
end;

constructor TOptixMemoryObject.Create(const ABase64Data : String);
begin
  if String.IsNullOrWhiteSpace(ABase64Data) then
    Create()
  else begin
    var ABytes := TNetEncoding.Base64.DecodeStringToBytes(ABase64Data);

    ///
    CopyFrom(@ABytes[0], Length(ABytes));
  end;
end;

destructor TOptixMemoryObject.Destroy();
begin
  FreeMemory();

  ///
  inherited Destroy();
end;

function TOptixMemoryObject.GetAsBase64() : String;
begin
  result := '';
  ///

  if Assigned(FAddress) and (FSize > 0) then
    result := TNetEncoding.Base64.EncodeBytesToString(FAddress, FSize);
end;

end.
