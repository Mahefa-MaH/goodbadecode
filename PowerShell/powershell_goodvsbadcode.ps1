# Good Code Example:  Using variables and functions for better readability and maintainability.

$files = Get-ChildItem -Path C:\Temp -Filter *.txt

function Test-File {
    param(
        [string]$FilePath
    )
    if (Test-Path $FilePath) {
        Write-Host "$FilePath exists."
    } else {
        Write-Host "$FilePath does not exist."
    }
}

foreach ($file in $files) {
    Test-File -FilePath $file.FullName
}


# Bad Code Example:  Lack of variables, inline commands, and poor readability.

Get-ChildItem C:\Temp *.txt | ForEach-Object {if (Test-Path $_.FullName){Write-Host "$_.FullName exists"} else {Write-Host "$_.FullName does not exist"}}
