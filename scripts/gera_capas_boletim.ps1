param(
  [string]$TemplatePath = "",
  [string]$OutputDir = "",
  [int]$Year = 2026
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem

function Get-FullPath {
  param([string]$PathValue)
  [System.IO.Path]::GetFullPath($PathValue)
}

function Remove-IfExists {
  param([string]$LiteralPath)
  if (Test-Path -LiteralPath $LiteralPath) {
    Remove-Item -LiteralPath $LiteralPath -Recurse -Force
  }
}

if ([string]::IsNullOrWhiteSpace($OutputDir)) {
  $OutputDir = Join-Path $PSScriptRoot "..\boletim"
}

if ([string]::IsNullOrWhiteSpace($TemplatePath)) {
  $TemplatePath = Join-Path $OutputDir "Boletim Mensal do Mercado de Trabalho.pptx"
}

$TemplatePath = Get-FullPath $TemplatePath
$OutputDir = Get-FullPath $OutputDir

if (!(Test-Path -LiteralPath $TemplatePath)) {
  throw "Template nao encontrado: $TemplatePath"
}

if (!(Test-Path -LiteralPath $OutputDir)) {
  throw "Pasta de saida nao encontrada: $OutputDir"
}

$mesMarco = "MAR" + [char]0x00C7 + "O"
$months = @(
  @{ Number = "01"; Slug = "jan"; Label = "JANEIRO DE $Year" },
  @{ Number = "02"; Slug = "fev"; Label = "FEVEREIRO DE $Year" },
  @{ Number = "03"; Slug = "mar"; Label = "$mesMarco DE $Year" },
  @{ Number = "04"; Slug = "abr"; Label = "ABRIL DE $Year" },
  @{ Number = "05"; Slug = "mai"; Label = "MAIO DE $Year" },
  @{ Number = "06"; Slug = "jun"; Label = "JUNHO DE $Year" },
  @{ Number = "07"; Slug = "jul"; Label = "JULHO DE $Year" },
  @{ Number = "08"; Slug = "ago"; Label = "AGOSTO DE $Year" },
  @{ Number = "09"; Slug = "set"; Label = "SETEMBRO DE $Year" },
  @{ Number = "10"; Slug = "out"; Label = "OUTUBRO DE $Year" },
  @{ Number = "11"; Slug = "nov"; Label = "NOVEMBRO DE $Year" },
  @{ Number = "12"; Slug = "dez"; Label = "DEZEMBRO DE $Year" }
)

$searchText = "FEVEREIRO DE 2026"
$tempRoot = Join-Path $OutputDir "_tmp_capas"
Remove-IfExists $tempRoot
New-Item -ItemType Directory -Path $tempRoot | Out-Null

$powerPoint = $null

try {
  $powerPoint = New-Object -ComObject PowerPoint.Application

  foreach ($month in $months) {
    $baseName = "capa_{0}{1}" -f $month.Slug, ($Year.ToString().Substring(2, 2))
    $workDir = Join-Path $tempRoot $baseName
    $extractDir = Join-Path $workDir "extract"
    $tempPptx = Join-Path $workDir ($baseName + ".pptx")
    $pdfOut = Join-Path $OutputDir ($baseName + ".pdf")

    Remove-IfExists $workDir
    New-Item -ItemType Directory -Path $extractDir -Force | Out-Null

    [System.IO.Compression.ZipFile]::ExtractToDirectory($TemplatePath, $extractDir)

    $slideXmlPath = Join-Path $extractDir "ppt\slides\slide1.xml"
    $slideXml = Get-Content -LiteralPath $slideXmlPath -Raw -Encoding UTF8

    if ($slideXml -notmatch [regex]::Escape($searchText)) {
      throw "Texto base '$searchText' nao encontrado em $slideXmlPath"
    }

    $slideXml = $slideXml.Replace($searchText, $month.Label)
    Set-Content -LiteralPath $slideXmlPath -Value $slideXml -Encoding UTF8

    Remove-IfExists $tempPptx
    [System.IO.Compression.ZipFile]::CreateFromDirectory(
      $extractDir,
      $tempPptx,
      [System.IO.Compression.CompressionLevel]::Optimal,
      $false
    )

    Remove-IfExists $pdfOut
    $presentation = $powerPoint.Presentations.Open($tempPptx, $false, $true, $false)
    try {
      $presentation.SaveAs($pdfOut, 32)
    }
    finally {
      $presentation.Close()
    }

    Write-Host ("Gerado: {0}" -f $pdfOut)
  }
}
finally {
  if ($powerPoint -ne $null) {
    $powerPoint.Quit()
    [void][System.Runtime.InteropServices.Marshal]::ReleaseComObject($powerPoint)
  }
  Remove-IfExists $tempRoot
  [GC]::Collect()
  [GC]::WaitForPendingFinalizers()
}
