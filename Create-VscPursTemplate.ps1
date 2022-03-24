[CmdletBinding()]
param (
    [string]$ProjectDir
)

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# Create src directory if it doesn't exist
if (!(Test-Path -Path (Join-Path $ProjectDir "src"))) {
    New-Item -ItemType Directory -Force -Path $ProjectDir -Name src
}

# Copy base files to the project directory
$templateDir = Join-Path $scriptDir "template"

Get-ChildItem -Path $templateDir -File -Recurse | ForEach-Object {
    $file = $_
    $relative = [System.IO.Path]::GetRelativePath($templateDir, $file)
    $dest = Join-Path $ProjectDir $relative
    if (!(Test-Path -Path $dest)) {
        Write-Host "Copying $file to $dest"
        Copy-Item $file $dest -Force -Recurse 
    }
    else {
        Write-Host "File $dest already exists"
    }
}

# Link file to VSCode PureScript API
$filesToLink = @(
    "src/VSCode"
)
$filesToLink | ForEach-Object {
    $file = Join-Path $scriptDir $_
    $dest = Join-Path $ProjectDir $_
    Write-Host "Linking $file"
    New-Item -ItemType SymbolicLink -Path $dest -Target $file -Force
}

function AddNPMPackageNoInstall {
    param (
        [string]$PackageName,
        [switch]$Dev
    )
    $devArg = $Dev ? "--save-dev" : "--save"
    Write-Host "Adding $PackageName to package.json"
    Invoke-Expression "npm install  --prefix $ProjectDir $devArg --package-lock-only --no-package-lock $PackageName"
}

AddNPMPackageNoInstall -PackageName "lodash"

$devPackages = @(
    "@types/vscode"
    "@types/node"
    "gulp"
    "webpack-stream"
    "gulp-typescript"
    "gulp-shell"
) | Join-String -Separator " "

AddNPMPackageNoInstall -PackageName $devPackages -Dev

# update gitignore
$gitignore = Join-Path $ProjectDir ".gitignore"
$gitignoreContent = Get-Content $gitignore

$toIgnore = @(
    "output"
    "src/VSCode"
    "dist"
    ".spago"
) 
Write-Host "Updating $gitignore with '$toIgnore'"
$gitignoreContent = ($gitignoreContent -split "`n") + $toIgnore | Select-Object -Unique | Join-String -Separator "`n"
Set-Content -Path $gitignore -Value $gitignoreContent

# Link the .spago directory to the project directory, to save space.
New-Item -Type SymbolicLink -Target (Join-Path $scriptDir .spago) -Path (Join-Path $ProjectDir ".spago") -Force

