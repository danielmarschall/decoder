unit DecoderSevenZipUtils;

interface

uses
  System.SysUtils, DecoderFuncs{$IFNDEF Console}, Fmx.Forms{$ENDIF};

procedure SevenZipFolder(const AFolderName, AArchFile: string; AOnProgress: TDcProgressEvent=nil); overload;
procedure SevenZipFolder(const AFolderName, AArchFile: string; AAlgo: TGuid; AOnProgress: TDcProgressEvent=nil); overload;

procedure SevenZipExtract(const AArchFile, AFolder: string; AOnProgress: TDcProgressEvent=nil); overload;
procedure SevenZipExtract(const AArchFile, AFolder: string; AAlgo: TGuid; AOnProgress: TDcProgressEvent=nil); overload;

function SevenZip_SupportedPackAlgo(const SevenZipAlgo: TGUID): boolean;
function SevenZip_SupportedUnPackAlgo(const SevenZipAlgo: TGUID): boolean;
function SevenZip_GetDefaultExtForAlgo(const SevenZipAlgo: TGUID; const AFallBack: string): string;
function SevenZip_GetAlgoFromFilename(const AFilename: string): TGUID;

const
  // taken from sevenzip.pas
  CLSID_CFormatZip      : TGUID = '{23170F69-40C1-278A-1000-000110010000}'; // [OUT] zip jar xpi
  CLSID_CFormatBZ2      : TGUID = '{23170F69-40C1-278A-1000-000110020000}'; // [OUT] bz2 bzip2 tbz2 tbz
  CLSID_CFormatRar      : TGUID = '{23170F69-40C1-278A-1000-000110030000}'; // [IN ] rar r00
  CLSID_CFormatArj      : TGUID = '{23170F69-40C1-278A-1000-000110040000}'; // [IN ] arj
  CLSID_CFormatZ        : TGUID = '{23170F69-40C1-278A-1000-000110050000}'; // [IN ] z taz
  CLSID_CFormatLzh      : TGUID = '{23170F69-40C1-278A-1000-000110060000}'; // [IN ] lzh lha
  CLSID_CFormat7z       : TGUID = '{23170F69-40C1-278A-1000-000110070000}'; // [OUT] 7z
  CLSID_CFormatCab      : TGUID = '{23170F69-40C1-278A-1000-000110080000}'; // [IN ] cab
  CLSID_CFormatNsis     : TGUID = '{23170F69-40C1-278A-1000-000110090000}'; // [IN ] nsis
  CLSID_CFormatLzma     : TGUID = '{23170F69-40C1-278A-1000-0001100A0000}'; // [IN ] lzma
  CLSID_CFormatLzma86   : TGUID = '{23170F69-40C1-278A-1000-0001100B0000}'; // [IN ] lzma 86
  CLSID_CFormatXz       : TGUID = '{23170F69-40C1-278A-1000-0001100C0000}'; // [OUT] xz
  CLSID_CFormatPpmd     : TGUID = '{23170F69-40C1-278A-1000-0001100D0000}'; // [IN ] ppmd
  CLSID_CFormatZStd     : TGUID = '{23170F69-40C1-278A-1000-0001100E0000}'; // zstd
  CLSID_CFormatLvm      : TGUID = '{23170F69-40C1-278A-1000-000110BF0000}'; // lvm
  CLSID_CFormatAVB      : TGUID = '{23170F69-40C1-278A-1000-000110C00000}';
  CLSID_CFormatLP       : TGUID = '{23170F69-40C1-278A-1000-000110C10000}';
  CLSID_CFormatSparse   : TGUID = '{23170F69-40C1-278A-1000-000110C20000}';
  CLSID_CFormatAPFS     : TGUID = '{23170F69-40C1-278A-1000-000110C30000}';
  CLSID_CFormatVhdx     : TGUID = '{23170F69-40C1-278A-1000-000110C40000}';
  CLSID_CFormatBase64   : TGUID = '{23170F69-40C1-278A-1000-000110C50000}';
  CLSID_CFormatCOFF     : TGUID = '{23170F69-40C1-278A-1000-000110C60000}';
  CLSID_CFormatExt      : TGUID = '{23170F69-40C1-278A-1000-000110C70000}'; // [IN ] ext
  CLSID_CFormatVMDK     : TGUID = '{23170F69-40C1-278A-1000-000110C80000}'; // [IN ] vmdk
  CLSID_CFormatVDI      : TGUID = '{23170F69-40C1-278A-1000-000110C90000}'; // [IN ] vdi
  CLSID_CFormatQcow     : TGUID = '{23170F69-40C1-278A-1000-000110CA0000}'; // [IN ] qcow
  CLSID_CFormatGPT      : TGUID = '{23170F69-40C1-278A-1000-000110CB0000}'; // [IN ] GPT
  CLSID_CFormatRar5     : TGUID = '{23170F69-40C1-278A-1000-000110CC0000}'; // [IN ] Rar5
  CLSID_CFormatIHex     : TGUID = '{23170F69-40C1-278A-1000-000110CD0000}'; // [IN ] IHex
  CLSID_CFormatHxs      : TGUID = '{23170F69-40C1-278A-1000-000110CE0000}'; // [IN ] Hxs
  CLSID_CFormatTE       : TGUID = '{23170F69-40C1-278A-1000-000110CF0000}'; // [IN ] TE
  CLSID_CFormatUEFIc    : TGUID = '{23170F69-40C1-278A-1000-000110D00000}'; // [IN ] UEFIc
  CLSID_CFormatUEFIs    : TGUID = '{23170F69-40C1-278A-1000-000110D10000}'; // [IN ] UEFIs
  CLSID_CFormatSquashFS : TGUID = '{23170F69-40C1-278A-1000-000110D20000}'; // [IN ] SquashFS
  CLSID_CFormatCramFS   : TGUID = '{23170F69-40C1-278A-1000-000110D30000}'; // [IN ] CramFS
  CLSID_CFormatAPM      : TGUID = '{23170F69-40C1-278A-1000-000110D40000}'; // [IN ] APM
  CLSID_CFormatMslz     : TGUID = '{23170F69-40C1-278A-1000-000110D50000}'; // [IN ] MsLZ
  CLSID_CFormatFlv      : TGUID = '{23170F69-40C1-278A-1000-000110D60000}'; // [IN ] FLV
  CLSID_CFormatSwf      : TGUID = '{23170F69-40C1-278A-1000-000110D70000}'; // [IN ] SWF
  CLSID_CFormatSwfc     : TGUID = '{23170F69-40C1-278A-1000-000110D80000}'; // [IN ] SWFC
  CLSID_CFormatNtfs     : TGUID = '{23170F69-40C1-278A-1000-000110D90000}'; // [IN ] NTFS
  CLSID_CFormatFat      : TGUID = '{23170F69-40C1-278A-1000-000110DA0000}'; // [IN ] FAT
  CLSID_CFormatMbr      : TGUID = '{23170F69-40C1-278A-1000-000110DB0000}'; // [IN ] MBR
  CLSID_CFormatVhd      : TGUID = '{23170F69-40C1-278A-1000-000110DC0000}'; // [IN ] VHD
  CLSID_CFormatPe       : TGUID = '{23170F69-40C1-278A-1000-000110DD0000}'; // [IN ] PE (Windows Exe)
  CLSID_CFormatElf      : TGUID = '{23170F69-40C1-278A-1000-000110DE0000}'; // [IN ] ELF (Linux Exe)
  CLSID_CFormatMachO    : TGUID = '{23170F69-40C1-278A-1000-000110DF0000}'; // [IN ] Mach-O
  CLSID_CFormatUdf      : TGUID = '{23170F69-40C1-278A-1000-000110E00000}'; // [IN ] iso
  CLSID_CFormatXar      : TGUID = '{23170F69-40C1-278A-1000-000110E10000}'; // [IN ] xar
  CLSID_CFormatMub      : TGUID = '{23170F69-40C1-278A-1000-000110E20000}'; // [IN ] mub
  CLSID_CFormatHfs      : TGUID = '{23170F69-40C1-278A-1000-000110E30000}'; // [IN ] HFS
  CLSID_CFormatDmg      : TGUID = '{23170F69-40C1-278A-1000-000110E40000}'; // [IN ] dmg
  CLSID_CFormatCompound : TGUID = '{23170F69-40C1-278A-1000-000110E50000}'; // [IN ] msi doc xls ppt
  CLSID_CFormatWim      : TGUID = '{23170F69-40C1-278A-1000-000110E60000}'; // [OUT] wim swm
  CLSID_CFormatIso      : TGUID = '{23170F69-40C1-278A-1000-000110E70000}'; // [IN ] iso
  CLSID_CFormatBkf      : TGUID = '{23170F69-40C1-278A-1000-000110E80000}'; // [IN ] BKF
  CLSID_CFormatChm      : TGUID = '{23170F69-40C1-278A-1000-000110E90000}'; // [IN ] chm chi chq chw hxs hxi hxr hxq hxw lit
  CLSID_CFormatSplit    : TGUID = '{23170F69-40C1-278A-1000-000110EA0000}'; // [IN ] 001
  CLSID_CFormatRpm      : TGUID = '{23170F69-40C1-278A-1000-000110EB0000}'; // [IN ] rpm
  CLSID_CFormatDeb      : TGUID = '{23170F69-40C1-278A-1000-000110EC0000}'; // [IN ] deb
  CLSID_CFormatCpio     : TGUID = '{23170F69-40C1-278A-1000-000110ED0000}'; // [IN ] cpio
  CLSID_CFormatTar      : TGUID = '{23170F69-40C1-278A-1000-000110EE0000}'; // [OUT] tar
  CLSID_CFormatGZip     : TGUID = '{23170F69-40C1-278A-1000-000110EF0000}'; // [OUT] gz gzip tgz tpz

implementation

{$IFDEF MsWindows}
{$WARN UNIT_PLATFORM OFF}
uses
  SevenZip;
{$WARN UNIT_PLATFORM ON}
{$ENDIF}

const
  // taken from Winapi.Windows.pas
  E_ABORT = HRESULT($80004004);

function SevenZip_SupportedPackAlgo(const SevenZipAlgo: TGUID): boolean;
begin
  // According to website "Packing / unpacking: 7z, XZ, BZIP2, GZIP, TAR, ZIP and WIM"
  // this fits with the "OUT" GUIDs from sevenzip.pas
  result :=
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatZip) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatBZ2) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormat7z) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatXz) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatWim) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatTar) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatGZip);
end;

function SevenZip_SupportedUnPackAlgo(const SevenZipAlgo: TGUID): boolean;
begin
  result :=
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatZip) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatBZ2) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatRar) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatArj) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatZ) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatLzh) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormat7z) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatCab) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatNsis) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatLzma) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatLzma86) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatXz) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatPpmd) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatZStd) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatLvm) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatAVB) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatLP) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatSparse) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatAPFS) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatVhdx) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatBase64) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatCOFF) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatExt) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatVMDK) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatVDI) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatQcow) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatGPT) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatRar5) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatIHex) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatHxs) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatTE) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatUEFIc) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatUEFIs) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatSquashFS) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatCramFS) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatAPM) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatMslz) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatFlv) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatSwf) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatSwfc) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatNtfs) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatFat) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatMbr) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatVhd) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatPe) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatElf) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatMachO) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatUdf) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatXar) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatMub) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatHfs) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatDmg) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatCompound) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatWim) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatIso) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatBkf) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatChm) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatSplit) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatRpm) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatDeb) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatCpio) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatTar) or
       IsEqualGuid(SevenZipAlgo, CLSID_CFormatGZip);
end;

function SevenZip_GetDefaultExtForAlgo(const SevenZipAlgo: TGUID; const AFallBack: string): string;
begin
  // TODO: Extend list of supported algos
  if IsEqualGUID(SevenZipAlgo, CLSID_CFormatZip) then
    result := '.zip'
  else if IsEqualGUID(SevenZipAlgo, CLSID_CFormatBZ2) then
    result := '.bz2'
  else if IsEqualGUID(SevenZipAlgo, CLSID_CFormat7z) then
    result := '.7z'
  else if IsEqualGUID(SevenZipAlgo, CLSID_CFormatXz) then
    result := '.xz'
  else if IsEqualGUID(SevenZipAlgo, CLSID_CFormatWim) then
    result := '.wim'
  else if IsEqualGUID(SevenZipAlgo, CLSID_CFormatTar) then
    result := '.tar'
  else if IsEqualGUID(SevenZipAlgo, CLSID_CFormatGZip) then
    result := '.gz'
  else
    result := AFallBack;
end;

function SevenZip_GetAlgoFromFilename(const AFilename: string): TGUID;
resourcestring
  SFileExtMissingForFolderPack = 'Cannot find compression algorithm given filename %s';
begin
  // TODO: Extend list of supported algos
  if AFilename.EndsWith('.zip', true) or AFilename.EndsWith('.jar', true) or AFilename.EndsWith('.xpi', true) then
    result := CLSID_CFormatZip
  else if AFilename.EndsWith('.bz2', true) or AFilename.EndsWith('.bzip2', true) or AFilename.EndsWith('.tbz2', true) or AFilename.EndsWith('.tbz', true) then
    result := CLSID_CFormatBZ2
  else if AFilename.EndsWith('.7z', true) then
    result := CLSID_CFormat7z
  else if AFilename.EndsWith('.xz', true) then
    result := CLSID_CFormatXz
  else if AFilename.EndsWith('.wim', true) or AFilename.EndsWith('.swm', true) then
    result := CLSID_CFormatWim
  else if AFilename.EndsWith('.tar', true) then
    result := CLSID_CFormatTar
  else if AFilename.EndsWith('.gz', true) or AFilename.EndsWith('.gzip', true) or AFilename.EndsWith('.tgz', true) or AFilename.EndsWith('.tpz', true) then
    result := CLSID_CFormatGZip
  else
    raise Exception.CreateResFmt(@SFileExtMissingForFolderPack, [AFilename]);
end;

type
  TSevenZipProgressContext = packed record
    DecProgress: TDcProgressEvent;
    Max: Int64;
    Task: string;
    StartSent: boolean;
    FinishedSent: boolean
  end;
  PSevenZipProgressContext = ^TSevenZipProgressContext;

function SevenZipProgress(sender: Pointer; total: boolean; value: int64): HRESULT; stdcall;
var
  state: TDcProgressState;
  docall: boolean;
begin
  if total then
  begin
    PSevenZipProgressContext(sender)^.Max := value;
  end
  else if Assigned(PSevenZipProgressContext(sender)^.DecProgress) then
  begin
    docall := true;
    if value = 0 then
    begin
      state := TDcProgressState.Started;
      PSevenZipProgressContext(sender)^.FinishedSent := false;
    end
    else if value = PSevenZipProgressContext(sender)^.Max then
    begin
      state := TDcProgressState.Finished;
    end
    else
    begin
      state := TDcProgressState.Processing;
    end;
    if state = TDcProgressState.Started then
    begin
      if PSevenZipProgressContext(sender)^.StartSent then
        state := TDcProgressState.Processing
      else
        PSevenZipProgressContext(sender)^.StartSent := true;
    end;
    if state = TDcProgressState.Finished then
    begin
      if PSevenZipProgressContext(sender)^.FinishedSent then
        docall := false // state := TDcProgressState.Processing
      else
        PSevenZipProgressContext(sender)^.FinishedSent := true;
    end;
    if docall then
      PSevenZipProgressContext(sender)^.DecProgress(PSevenZipProgressContext(sender)^.Max, value, PSevenZipProgressContext(sender)^.Task, state);
  end;
  {$IFDEF Console}
  Result := S_OK;
  {$ELSE}
  if Assigned(Application) and Application.Terminated then
    Result := E_ABORT
  else
    Result := S_OK;
  {$ENDIF}
end;

{$IFDEF MsWindows}
function SevenZipGetDll: string;
resourcestring
  SDllFileNotFound = 'File %s not found. Therefore, cannot compress or uncompress folders.';
begin
  // Extract 7z.dll from the 32 an 64 bit installer binaries
  // Do not take the 7za.dll ones from the extra archive
  {$IFDEF Win64}
  result := '7z.64.dll';
  {$ELSE}
  result := '7z.32.dll';
  {$ENDIF}
  if not FileExists(Result) then
    raise Exception.CreateResFmt(@SDllFileNotFound, [result]);
end;
{$ENDIF}

procedure SevenZipFolder(const AFolderName, AArchFile: string; AOnProgress: TDcProgressEvent=nil);
var
  Algo: TGuid;
begin
  Algo := SevenZip_GetAlgoFromFilename(AarchFile);
  SevenZipFolder(AFolderName, AArchFile, Algo, AOnProgress);
end;

procedure SevenZipFolder(const AFolderName, AArchFile: string; AAlgo: TGuid; AOnProgress: TDcProgressEvent=nil);
{$IFDEF MsWindows}
var
  Arch: I7zOutArchive;
  ProgressCtx: TSevenZipProgressContext;
resourcestring
  SPackFolderTask = '7zip Pack folder';
begin
  FillChar(ProgressCtx, Sizeof(ProgressCtx), 0);
  ProgressCtx.Task := LoadResString(@SPackFolderTask);
  ProgressCtx.DecProgress := AOnProgress;
  Arch := CreateOutArchive(AAlgo, SevenZipGetDll);
  Arch.AddFiles(AFolderName, '', '*', true);
  SetCompressionLevel(Arch, 5);
  SevenZipSetCompressionMethod(Arch, m7BZip2);
  Arch.SetProgressCallback(@ProgressCtx, SevenZipProgress);
  Arch.SaveToFile(AArchFile);
{$ELSE}
resourcestring
  SNo7zImplemented = '7zip-Pack/Unpack is not implemented for this Operating System.';
begin
  raise Exception.CreateRes(@SNo7zImplemented);
{$ENDIF}
end;

procedure SevenZipExtract(const AArchFile, AFolder: string; AOnProgress: TDcProgressEvent=nil);
var
  Algo: TGuid;
begin
  Algo := SevenZip_GetAlgoFromFilename(AarchFile);
  SevenZipExtract(AArchFile, AFolder, Algo, AOnProgress);
end;

procedure SevenZipExtract(const AArchFile, AFolder: string; AAlgo: TGuid; AOnProgress: TDcProgressEvent=nil);
{$IFDEF MsWindows}
var
  Arch: I7zInArchive;
  ProgressCtx: TSevenZipProgressContext;
resourcestring
  SUnpackFolderTask = '7zip Unpack folder';
begin
  FillChar(ProgressCtx, Sizeof(ProgressCtx), 0);
  ProgressCtx.Task := LoadResString(@SUnpackFolderTask);
  ProgressCtx.DecProgress := AOnProgress;
  Arch := CreateInArchive(AAlgo, SevenZipGetDll);
  Arch.SetProgressCallback(@ProgressCtx, SevenZipProgress);
  Arch.OpenFile(AArchFile);
  Arch.ExtractTo(AFolder);
{$ELSE}
resourcestring
  SNo7zImplemented = '7zip-Pack/Unpack is not implemented for this Operating System.';
begin
  raise Exception.CreateRes(@SNo7zImplemented);
{$ENDIF}
end;

end.
