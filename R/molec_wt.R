#' @title Identify Molecular Weight for a Given Element
#' 
#' @description Identifies molecular weight for the specified element based on the element's name, its symbol, or its atomic number. Returns only the molecular weight as a numeric value.
#' 
#' @param element (character/numeric) element name, symbol, or atomic number for which to retrieve molecular weight
#' 
#' @importFrom magrittr %>%
#' 
#' @return (numeric) molecular weight value for the relevant element
#' 
#' @export
#' 
#' @examples
#' # Identify molecular weight for carbon by name
#' molec_wt(element = "Carbon")
#' 
#' # Identify molecular weight for hydrogen by atomic number
#' molec_wt(element = 1)
#' 
molec_wt <- function(element = NULL){
  
  # Squelch 'visible bindings' NOTE
  atomic_number <- element_name <- symbol <- molecular_weight <- NULL
  
  # Error out for no argument specification
  if(is.null(element) == TRUE)
    stop("Element must be provided")
  
  # Error out for invalid user entry
  if ((is.numeric(element) == TRUE & (! element %in% 1:118)))
    stop("Input must be a valid atomic number")
  
  # Coerce user entry to lowercase (if text)
  if(is.character(element) == TRUE){
    element <- tolower(x = element)
  }
  
  # Define table of molecular weight information
  element_table <- data.frame(atomic_number = 1:118, 
                              element_name = c("Hydrogen", "Helium", "Lithium", "Beryllium", "Boron", "Carbon", "Nitrogen", "Oxygen", "Fluorine", "Neon", "Sodium", "Magnesium", "Aluminum", "Silicon", "Phosphorus", "Sulfur", "Chlorine", "Argon", "Potassium", "Calcium", "Scandium", "Titanium", "Vanadium", "Chromium", "Manganese", "Iron", "Cobalt", "Nickel", "Copper", "Zinc", "Gallium", "Germanium", "Arsenic", "Selenium", "Bromine", "Krypton", "Rubidium", "Strontium", "Yttrium", "Zirconium", "Niobium", "Molybdenum", "Technetium", "Ruthenium", "Rhodium", "Palladium", "Silver", "Cadmium", "Indium", "Tin", "Antimony", "Tellurium", "Iodine", "Xenon", "Cesium", "Barium", "Lanthanum", "Cerium", "Praseodymium", "Neodymium", "Promethium", "Samarium", "Europium", "Gadolinium", "Terbium", "Dysprosium", "Holmium", "Erbium", "Thulium", "Ytterbium", "Lutetium", "Hafnium", "Tantalum", "Wolfram", "Rhenium", "Osmium", "Iridium", "Platinum", "Gold", "Mercury", "Thallium", "Lead", "Bismuth", "Polonium", "Astatine", "Radon", "Francium", "Radium", "Actinium", "Thorium", "Protactinium", "Uranium", "Neptunium", "Plutonium", "Americium", "Curium", "Berkelium", "Californium", "Einsteinium", "Fermium", "Mendelevium", "Nobelium", "Lawrencium", "Rutherfordium", "Dubnium", "Seaborgium", "Bohrium", "Hassium", "Meitnerium", "Darmstadtium", "Roentgenium", "Copernicium", "Nihonium", "Flerovium", "Moscovium", "Livermorium", "Tennessine", "Oganesson"),
                              symbol = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og"),
                              molecular_weight = c(1.007, 4.002, 6.941, 9.012, 10.811, 12.011, 14.007, 15.999, 18.998, 20.18, 22.99, 24.305, 26.982, 28.086, 30.974, 32.065, 35.453, 39.948, 39.098, 40.078, 44.956, 47.867, 50.942, 51.996, 54.938, 55.845, 58.933, 58.693, 63.546, 65.38, 69.723, 72.64, 74.922, 78.96, 79.904, 83.798, 85.468, 87.62, 88.906, 91.224, 92.906, 95.96, 98, 101.07, 102.906, 106.42, 107.868, 112.411, 114.818, 118.71, 121.76, 127.6, 126.904, 131.293, 132.905, 137.327, 138.905, 140.116, 140.908, 144.242, 145, 150.36, 151.964, 157.25, 158.925, 162.5, 164.93, 167.259, 168.934, 173.054, 174.967, 178.49, 180.948, 183.84, 186.207, 190.23, 192.217, 195.084, 196.967, 200.59, 204.383, 207.2, 208.98, 210, 210, 222, 223, 226, 227, 232.038, 231.036, 238.029, 237, 244, 243, 247, 247, 251, 252, 257, 258, 259, 262, 261, 262, 266, 264, 267, 268, 271, 272, 285, 284, 289, 288, 292, 295, 294))
  
  # Error out for invalid user entry
  if ((is.character(element) == TRUE & (! element %in% tolower(element_table$element_name))))
    stop("Input must be a valid element name")
  
  # Filter to user entry
  weight_actual <- element_table %>%
    dplyr::filter(atomic_number == element |
                    tolower(element_name) == element |
                    tolower(symbol) == element) %>%
    # Pull out just the weight
    dplyr::pull(molecular_weight)
  
  # Return that
  return(weight_actual)
}
