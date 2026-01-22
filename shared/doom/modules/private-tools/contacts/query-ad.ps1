param(
    [Parameter(Mandatory=$true)]
    [string]$SearchTerm
)

# 1. Force UTF-8 encoding so Emacs reads special characters correctly
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

# 2. Configure the searcher
$searcher = [adsisearcher]"(anr=$SearchTerm)"
# Add specific properties you want. Remove this line to fetch *everything* (slower).
$searcher.PropertiesToLoad.AddRange(@('uid', 'cn', 'title', 'mail', 'telephoneNumber', 'department', 'manager', 'sAMAccountName'))

$results = $searcher.FindAll()

if ($results.Count -eq 0) {
    # Return an empty JSON list
    Write-Output "[]"
    exit
}

$outputList = @()

foreach ($res in $results) {
    $obj = @{}
    $props = $res.Properties
    foreach ($key in $props.PropertyNames) {
        # FIX: Flatten the AD Collection to a string
        $obj[$key] = $props[$key] -join '; '
    }
    if ($obj.ContainsKey('samaccountname')) {
	    $obj['uid'] = $obj['samaccountname']
    }
    if ($obj.ContainsKey('uid')) {
	    $outputList += $obj
    }
}

# 3. Convert to JSON
# We wrap $outputList in @() to ensure it's always an array, 
# even if only one result is found.
Write-Output (ConvertTo-Json -InputObject @($outputList) -Depth 2 -Compress)
