fitness_value <- function(fm, nfm, age, om, nom, fom, oa, ns, nl){
    
    # If female mass & pup mass < critical,            fitness = 0
    # If female mass < critical & pup mass > critical, fitness = pup recruitment
    # If female mass > critical & pup mass < critical, fitness = non-lact
    # If female mass & pup mass > critical,            fitness = lact
    
    # fm = female mass
    # nfm = new female mass
    # om = offspring mass
    # nom = new offspring mass
    # age = female age
    # oa = offspring age
    # ns = state
    # nl = location
    
    
    ifelse(fm < critical_mass(age) & om < critical_mass(oa), 0, 
           ifelse(fm < critical_mass(age) & om > critical_mass(oa), recruitment(ifelse(fom < maximum_mass(oa), fom, maximum_mass(oa)), oa),
                  ifelse(fm > critical_mass(age) & om < critical_mass(oa), fitness_nonlac(fm = nfm, t = age, newstate = 1, newloc = nl),
                         fitness_lac(fm = nfm, om = ifelse(nom < maximum_mass(oa), nom, maximum_mass(oa)), oom = om, t = age, oa = oa, newstate = ns, newloc = nl))))
    
}
