[CmdletBinding()]
param (
    [Parameter()]
    [string] $J1,
    [Parameter()]
    [string] $J2
)

function Join-Objects($source, $extend) {
    if ($source.GetType().Name -eq "PSCustomObject" -and $extend.GetType().Name -eq "PSCustomObject") {
        foreach ($Property in $source | Get-Member -type NoteProperty, Property) {
            if ($null -eq $extend.$($Property.Name)) {
                continue;
            }
            $source.$($Property.Name) = Join-Objects $source.$($Property.Name) $extend.$($Property.Name)
        }
    }
    elseif($source -is [array] -and $extend -is [array]) {
        $source = $source + $extend
    }
    else {
        $source = $extend;
    }
    return $source
}
function AddPropertyRecurse($source, $toExtend) {
    if ($source.GetType().Name -eq "PSCustomObject") {
        foreach ($Property in $source | Get-Member -type NoteProperty, Property) {
            if ($null -eq $toExtend.$($Property.Name)) {
                $toExtend | Add-Member -MemberType NoteProperty -Value $source.$($Property.Name) -Name $Property.Name `
            
            }
            else {
                $toExtend.$($Property.Name) = AddPropertyRecurse $source.$($Property.Name) $toExtend.$($Property.Name)
            }
        }
    }
    return $toExtend
}
function Merge-Json($source, $extend) {
    $merged = Join-Objects $source $extend
    $extended = AddPropertyRecurse $extend $merged
    return $extended
}

function Merge-JsonFiles($J1, $J2) {
    $1 = Get-Content $J1 -Raw | ConvertFrom-Json
    $2 = Get-Content $J2 -Raw | ConvertFrom-Json
    Merge-Json $1 $2 | ConvertTo-Json -Depth 100
}

$J1 = ".\js1.json"
$J2 = ".\js2.json"
$out = Merge-JsonFiles $J1 $J2
Write-Host $out