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
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); virtual;
  public
    {@M}
    function Serialize() : ISuperObject; virtual;

    {@C}
    constructor Create(); overload; virtual;
    constructor Create(const ASerializedObject : ISuperObject); overload; virtual;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Winapi.Windows,

  System.Rtti, System.TypInfo, System.SysUtils;
// ---------------------------------------------------------------------------------------------------------------------

{ TOptixSerializableObject.Serialize }
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
          end;
          // -----------------------------------------------------------------------------------------------------------
        end;
        // Sets --------------------------------------------------------------------------------------------------------
        tkSet :
          result.S[AField.Name] := SetToString(AField.FieldType.Handle, AField.GetValue(self).GetReferenceToRawData);
        // Integers ----------------------------------------------------------------------------------------------------
        tkInteger, tkFloat, tkInt64 :
          result.I[AField.Name] := AField.GetValue(self).AsInteger();
        // Enum --------------------------------------------------------------------------------------------------------
        tkEnumeration :
          result.I[AField.Name] := AField.GetValue(self).AsOrdinal();
        // Strings -----------------------------------------------------------------------------------------------------
        tkChar, tkString, tkWChar, tkLString, tkWString, tkUString :
          result.S[AField.Name] := AField.GetValue(self).AsString();
        // -------------------------------------------------------------------------------------------------------------
      end;
    end;
  end;
end;

continuer l'implémentation de la serialization (semi-auto), ajouter le support de TDateTime

{ TOptixSerializableObject.DeSerialize }
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
        if not Assigned(AObject) then
          continue;
        ///

        // Serializable Object -----------------------------------------------------------------------------------------
        if AFieldClass.InheritsFrom(TOptixSerializableObject) then begin
          var ASerializableObject := AObject as TOptixSerializableObject;

          ///
          ASerializableObject.DeSerialize(APair.AsObject);
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
          StringToSet(AField.FieldType.Handle, APair.AsString, Pointer(NativeUInt(self) + AField.Offset));
        except
        end;
      // ---------------------------------------------------------------------------------------------------------------
    end;
  end;
end;

{ TOptixSerializableObject.Create }
constructor TOptixSerializableObject.Create();
begin
  inherited;
  ///


end;

{ TOptixSerializableObject.Create }
constructor TOptixSerializableObject.Create(const ASerializedObject : ISuperObject);
begin
  Create();
  ///

  if Assigned(ASerializedObject) then
    DeSerialize(ASerializedObject);
end;

end.
