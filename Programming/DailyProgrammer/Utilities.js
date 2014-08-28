var Utilities = {
    function resetAll(name) {           //Resets all values in textboxes of class 'name'
        var elements = [];
        elements = document.getElementsByClassName(name);
        for(var i=0; i<elements.length; i++) {
            elements[i].value = '';
        };
    };

    function swapElements(a,b) {        //Swaps 2 elements in an array
        var temp = a;
        a = b;
        b = temp;
    };

    function documentAppender(a,b) {    //Appends a string to a document
        var h=document.createElement(a);
        var t=document.createTextNode(b);
        h.appendChild(t);
        document.body.appendChild(h);
    };

    function quadratic(a,b,c) {         //Quadratic equation solver
        var d = (b*b)-(4*a*c);
        if(d>0) {
            var x1 = ((0-b)+Math.sqrt(d))/(2*a);
            var x2 = ((0-b)-Math.sqrt(d))/(2*a);
            if(x1>=0&&x2<0){
                return x1;
            } else if(x1<0&&x2>=0){
                return x2;
            };
        } else if(d=0) {
            var x = (0-b)/(2*a);
            return x;
        } else {
            return NaN;
        };
    };

    var chemicalSymbols = ['H','He','Li','Be','B','C','N','O','F','Ne','Na','Mg','Al','Si','P','S','Cl','Ar','K','Ca','Sc','Ti','V','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn','Sb','Te','I','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W','Re','Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk','Cf','Es','Fm','Cf','No','Lr','Rf','Db','Sg','Bh','Hs','Mt','Ds','Rg','Cn','Uut','Uuq','Uup','Uuh','Uus','Uuo'];

    var chemicalNames = ['Hydrogen','Helium','Lithium','Beryllium','Boron','Carbon','Nitrogen','Oxygen','Fluorine','Neon','Sodium','Magnesium','Aluminium','Silicon','Phosphorus','Sulphur','Chlorine','Argon','Potassium','Calcium','Scandium','Titanium','Vanadium','Chromium','Manganese','Iron','Cobalt','Nickel','Copper','Zinc','Gallium','Germanium','Arsenic','Selenium','Bromine','Krypton','Rubidium','Strontium','Yttrium','Zirconium','Niobium','Molybdenum','Technetium','Rubidium','Rhodium','Palladadium','Silver','Cadmium','Indium','Tin','Antimony','Tellurium','Iodine','Xenon','Caesium','Barium','Lanthanum','Cerium','Promethium','Neodymium','Promethium','Samarium','Europium','Gadolinium','Terbium','Dysprosium','Holmium','Erbium','Thulium','Ytterbium','Lutetium','Hafnium','Tantalum','Tungsten','Rhenium','Osmium','Iridium','Platinum','Gold','Mercury','Thallium','Lead','Bismuth','Polonium','Astatine','Radon','Francium','Radium','Actinium','Thorium','Protactinium','Uranium','Neptunium','Plutonium','Americium','Curium','Berkelium','Californium','Einsteinium','Fermiumm','Mendelevium','Nobelium','Lawrentium','Rutherfordium','Dubnium','Seaborgium','Bohrium','Hassium','Meitnerium','Darmstadtium','Roentgenium','Coperniciumn','Ununtritium','Ununquadium','Ununppentium','Ununhexium','Ununseptium','Ununoctium'];

    var periodicTable = {
        1: {
            name: "Hydrogen{",
            protonnumber: 1,
            symbol: "H",
        },
        2: {
            name: "Helium",
            protonnumber: 2,
            symbol: "He",
        },
        3: {
            name: "Lithium",
            protonnumber: 3,
            symbol: "Li",
        },
        4: {
            name: "Beryllium",
            protonnumber: 4,
            symbol: "Be",
        },
        5: {
            name: "Boron",
            protonnumber: 5,
            symbol: "B",
        },
        6: {
            name: "Carbon",
            protonnumber: 6,
            symbol: "C",
        },
        7: {
            name: "Nitrogen",
            protonnumber: 7,
            symbol: "N",
        },
        8: {
            name: "Oxygen",
            protonnumber: 8,
            symbol: "O",
        },
        9: {
            name: "Fluorine",
            protonnumber: 9,
            symbol: "F",
        },
        10: {
            name: "Neon",
            protonnumber: 10,
            symbol: "Ne",
        },
        11: {
            name: "Sodium",
            protonnumber: 11,
            symbol: "Na",
        },
        12: {
            name: "Magnesium",
            protonnumber: 12,
            symbol: "Mg",
        },
        13: {
            name: "Aluminium",
            protonnumber: 13,
            symbol: "Al",
        },
        14: {
            name: "Silicon",
            protonnumber: 14,
            symbol: "Si",
        },
        15: {
            name: "Phosphorus",
            protonnumber: 15,
            symbol: "P",
        },
        16: {
            name: "Sulphur",
            protonnumber: 16,
            symbol: "S",
        },
        17: {
            name: "Chlorine",
            protonnumber: 17,
            symbol: "Cl",
        },
        18: {
            name: "Argon",
            protonnumber: 18,
            symbol: "Ar",
        },
        19: {
            name: "Potassium",
            protonnumber: 19,
            symbol: "K",
        },
        20: {
            name: "Calcium",
            protonnumber: 20,
            symbol: "Ca",
        },
        21: {
            name: "Scandium",
            protonnumber: 21,
            symbol: "Sc",
        },
        22: {
            name: "Titanium",
            protonnumber: 22,
            symbol: "Ti",
        },
        23: {
            name: "Vanadium",
            protonnumber: 23,
            symbol: "V",
        },
        24: {
            name: "Chromium",
            protonnumber: 24,
            symbol: "Cr",
        },
        25: {
            name: "Manganese",
            protonnumber: 25,
            symbol: "Mn",
        },
        26: {
            name: "Iron",
            protonnumber: 26,
            symbol: "Fe",
        },
        27: {
            name: "Cobalt",
            protonnumber: 27,
            symbol: "Co",
        },
        28: {
            name: "Nickel",
            protonnumber: 28,
            symbol: "Ni",
        },
        29: {
            name: "Copper",
            protonnumber: 29,
            symbol: "Cu",
        },
        30: {
            name: "Zinc",
            protonnumber: 30,
            symbol: "Zn",
        },
        31: {
            name: "Gallium",
            protonnumber: 31,
            symbol: "Ga",
        },
        32: {
            name: "Germanium",
            protonnumber: 32,
            symbol: "Ge",
        },
        33: {
            name: "Arsenic",
            protonnumber: 33,
            symbol: "As",
        },
        34: {
            name: "Selenium",
            protonnumber: 34,
            symbol: "Se",
        },
        35: {
            name: "Bromine",
            protonnumber: 35,
            symbol: "Br",
        },
        36: {
            name: "Krypton",
            protonnumber: 36,
            symbol: "Kr",
        },
        37: {
            name: "Rubidium",
            protonnumber: 37,
            symbol: "Rb",
        },
        38: {
            name: "Strontium",
            protonnumber: 38,
            symbol: "Sr",
        },
        39: {
            name: "Yttrium",
            protonnumber: 39,
            symbol: "Y",
        },
        40: {
            name: "Zirconium",
            protonnumber: 40,
            symbol: "Zr",
        },
        41: {
            name: "Niobium",
            protonnumber: 41,
            symbol: "Nb",
        },
        42: {
            name: "Molybdenum",
            protonnumber: 42,
            symbol: "Mo",
        },
        43: {
            name: "Technetium",
            protonnumber: 43,
            symbol: "Tc",
        },
        44: {
            name: "Rubidium",
            protonnumber: 44,
            symbol: "Ru",
        },
        45: {
            name: "Rhodium",
            protonnumber: 45,
            symbol: "Rh",
        },
        46: {
            name: "Palladadium",
            protonnumber: 46,
            symbol: "Pd",
        },
        47: {
            name: "Silver",
            protonnumber: 47,
            symbol: "Ag",
        },
        48: {
            name: "Cadmium",
            protonnumber: 48,
            symbol: "Cd",
        },
        49: {
            name: "Indium",
            protonnumber: 49,
            symbol: "In",
        },
        50: {
            name: "Tin",
            protonnumber: 50,
            symbol: "Sn",
        },
        51: {
            name: "Antimony",
            protonnumber: 51,
            symbol: "Sb",
        },
        52: {
            name: "Tellurium",
            protonnumber: 52,
            symbol: "Te",
        },
        53: {
            name: "Iodine",
            protonnumber: 53,
            symbol: "I",
        },
        54: {
            name: "Xenon",
            protonnumber: 54,
            symbol: "Xe",
        },
        55: {
            name: "Caesium",
            protonnumber: 55,
            symbol: "Cs",
        },
        56: {
            name: "Barium",
            protonnumber: 56,
            symbol: "Ba",
        },
        57: {
            name: "Lanthanum",
            protonnumber: 57,
            symbol: "La",
        },
        58: {
            name: "Cerium",
            protonnumber: 58,
            symbol: "Ce",
        },
        59: {
            name: "Promethium",
            protonnumber: 59,
            symbol: "Pr",
        },
        60: {
            name: "Neodymium",
            protonnumber: 60,
            symbol: "Nd",
        },
        61: {
            name: "Promethium",
            protonnumber: 61,
            symbol: "Pm",
        },
        62: {
            name: "Samarium",
            protonnumber: 62,
            symbol: "Sm",
        },
        63: {
            name: "Europium",
            protonnumber: 63,
            symbol: "Eu",
        },
        64: {
            name: "Gadolinium",
            protonnumber: 64,
            symbol: "Gd",
        },
        65: {
            name: "Terbium",
            protonnumber: 65,
            symbol: "Tb",
        },
        66: {
            name: "Dysprosium",
            protonnumber: 66,
            symbol: "Dy",
        },
        67: {
            name: "Holmium",
            protonnumber: 67,
            symbol: "Ho",
        },
        68: {
            name: "Erbium",
            protonnumber: 68,
            symbol: "Er",
        },
        69: {
            name: "Thulium",
            protonnumber: 69,
            symbol: "Tm",
        },
        70: {
            name: "Ytterbium",
            protonnumber: 70,
            symbol: "Yb",
        },
        71: {
            name: "Lutetium",
            protonnumber: 71,
            symbol: "Lu",
        },
        72: {
            name: "Hafnium",
            protonnumber: 72,
            symbol: "Hf",
        },
        73: {
            name: "Tantalum",
            protonnumber: 73,
            symbol: "Ta",
        },
        74: {
            name: "Tungsten",
            protonnumber: 74,
            symbol: "W",
        },
        75: {
            name: "Rhenium",
            protonnumber: 75,
            symbol: "Re",
        },
        76: {
            name: "Osmium",
            protonnumber: 76,
            symbol: "Os",
        },
        77: {
            name: "Iridium",
            protonnumber: 77,
            symbol: "Ir",
        },
        78: {
            name: "Platinum",
            protonnumber: 78,
            symbol: "Pt",
        },
        79: {
            name: "Gold",
            protonnumber: 79,
            symbol: "Au",
        },
        80: {
            name: "Mercury",
            protonnumber: 80,
            symbol: "Hg",
        },
        81: {
            name: "Thallium",
            protonnumber: 81,
            symbol: "Tl",
        },
        82: {
            name: "Lead",
            protonnumber: 82,
            symbol: "Pb",
        },
        83: {
            name: "Bismuth",
            protonnumber: 83,
            symbol: "Bi",
        },
        84: {
            name: "Polonium",
            protonnumber: 84,
            symbol: "Po",
        },
        85: {
            name: "Astatine",
            protonnumber: 85,
            symbol: "At",
        },
        86: {
            name: "Radon",
            protonnumber: 86,
            symbol: "Rn",
        },
        87: {
            name: "Francium",
            protonnumber: 87,
            symbol: "Fr",
        },
        88: {
            name: "Radium",
            protonnumber: 88,
            symbol: "Ra",
        },
        89: {
            name: "Actinium",
            protonnumber: 89,
            symbol: "Ac",
        },
        90: {
            name: "Thorium",
            protonnumber: 90,
            symbol: "Th",
        },
        91: {
            name: "Protactinium",
            protonnumber: 91,
            symbol: "Pa",
        },
        92: {
            name: "Uranium",
            protonnumber: 92,
            symbol: "U",
        },
        93: {
            name: "Neptunium",
            protonnumber: 93,
            symbol: "Np",
        },
        94: {
            name: "Plutonium",
            protonnumber: 94,
            symbol: "Pu",
        },
        95: {
            name: "Americium",
            protonnumber: 95,
            symbol: "Am",
        },
        96: {
            name: "Curium",
            protonnumber: 96,
            symbol: "Cm",
        },
        97: {
            name: "Berkelium",
            protonnumber: 97,
            symbol: "Bk",
        },
        98: {
            name: "Californium",
            protonnumber: 98,
            symbol: "Cf",
        },
        99: {
            name: "Einsteinium",
            protonnumber: 99,
            symbol: "Es",
        },
        100: {
            name: "Fermiumm",
            protonnumber: 100,
            symbol: "Fm",
        },
        101: {
            name: "Mendelevium",
            protonnumber: 101,
            symbol: "Cf",
        },
        102: {
            name: "Nobelium",
            protonnumber: 102,
            symbol: "No",
        },
        103: {
            name: "Lawrentium",
            protonnumber: 103,
            symbol: "Lr",
        },
        104: {
            name: "Rutherfordium",
            protonnumber: 104,
            symbol: "Rf",
        },
        105: {
            name: "Dubnium",
            protonnumber: 105,
            symbol: "Db",
        },
        106: {
            name: "Seaborgium",
            protonnumber: 106,
            symbol: "Sg",
        },
        107: {
            name: "Bohrium",
            protonnumber: 107,
            symbol: "Bh",
        },
        108: {
            name: "Hassium",
            protonnumber: 108,
            symbol: "Hs",
        },
        109: {
            name: "Meitnerium",
            protonnumber: 109,
            symbol: "Mt",
        },
        110: {
            name: "Darmstadtium",
            protonnumber: 110,
            symbol: "Ds",
        },
        111: {
            name: "Roentgenium",
            protonnumber: 111,
            symbol: "Rg",
        },
        112: {
            name: "Coperniciumn",
            protonnumber: 112,
            symbol: "Cn",
        },
        113: {
            name: "Ununtritium",
            protonnumber: 113,
            symbol: "Uut",
        },
        114: {
            name: "Ununquadium",
            protonnumber: 114,
            symbol: "Uuq",
        },
        115: {
            name: "Ununpentium",
            protonnumber: 115,
            symbol: "Uup",
        },
        116: {
            name: "Ununhexium",
            protonnumber: 116,
            symbol: "Uuh",
        },
        117: {
            name: "Ununseptium",
            protonnumber: 117,
            symbol: "Uus",
        },
        118: {
            name: "Ununoctium",
            protonnumber: 118,
            symbol: "Uuo",
        },
    };

    var lettersLower = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];

    var lettersUpper = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'];

    var numbers = [1,2,3,4,5,6,7,8,9,0];

    var arrowCodes = {          //Codes as interpreted by jQuery
        w:87,
        a:65,
        s:83,
        d:68,
    };
};
