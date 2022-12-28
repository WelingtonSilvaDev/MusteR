#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= r3dmol -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

mol3d<- function(pdb_view, ligid){
library(r3dmol)
r3dmol(
  viewer_spec = m_viewer_spec(
    cartoonQuality = 10,
    antialias = TRUE, 
    lowerZoomLimit = 50,
    upperZoomLimit = 350
  ),
  # id = "demo", elementId = "demo"
)%>%
  # Add model to scene
  m_add_model(data = paste0("pdb-mol/", pdb_view), format = "pdb") %>%
  # Zoom to encompass the whole scene
  m_zoom_to() %>% 
  # Set style of structures
  m_set_style(style = m_style_stick( radius = 0.1)) %>%
  # Set style of specific selection (selecting by secondary)
  m_set_style(
        sel = m_sel(resi = ligid), #resn = "ALD" elem="HETATM"
        style = m_style_stick(
          radius = 0.3,
          colorScheme = "magenta"
        ))%>%
  m_set_style(
    sel = m_sel(resn = "HOH"), #resn = "ALD" elem="HETATM"
    style = m_style_sphere(radius = 0.8))%>%
  m_add_surface(style = m_style_surface(opacity = 0.4, colorScheme = "ssPyMOL")) %>%
  #Style the alpha helix
  m_set_style(
    sel = m_sel(ss = "s"), # Style alpha helix
    #style = m_style_stick(color = "#ff7f0e")
    style = m_style_cartoon(color = "#ff7f0e")
  ) %>%
  m_add_res_labels(style = m_style_label(
    fontSize = 9,
    fontColor = "Blue",
    fontOpacity = 0.7,
    showBackground = F
  )) %>%
  # Rotate the scene by given angle on given axis
  m_rotate(angle = 90, axis = "y", animationDuration = 500) %>%
  # Animate the scene by spinning it
  m_spin(speed = 0) 
  
}

