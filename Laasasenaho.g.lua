local mdef = function(f, func) return string.format("R::%s.r::%s", f, func) end

model.Laasasenaho_stemcurve_pine {
    params  = { "tree#diameter", "tree#heigth", "tree#saw", "tree#pulp" },
    checks  = { ["tree#spe"] = "manty" },
    returns ={ "tree#totvol", "tree#sawvol", "tree#pulpvol", "tree#wastevol" },
    coeffs  = { "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"},
    impl    = mdef("Laasasenaho.R", "Laasasenaho_stemcurve_pine")
}

model.Laasasenaho_stemcurve_spruce {
    params  = { "tree#diameter", "tree#heigth", "tree#saw", "tree#pulp" },
    checks  = { ["tree#spe"] = "kuusi" },
    returns ={ "tree#totvol", "tree#sawvol", "tree#pulpvol", "tree#wastevol" },
    coeffs  = { "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"},
    impl    = mdef("Laasasenaho.R", "Laasasenaho_stemcurve_spruce")
}

model.Laasasenaho_stemcurve_birch {
    params  = { "tree#diameter", "tree#heigth"},
    user = { "tree#saw", "tree#pulp" },
    checks  = { ["tree#spe"] = "koivu" },
    returns ={ "tree#totvol", "tree#sawvol", "tree#pulpvol", "tree#wastevol" },
    coeffs  = { "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"},
    impl    = mdef("Laasasenaho.R", "Laasasenaho_stemcurve_birch")
}