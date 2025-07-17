local app = tui.newApp()
local text = tui.newTextView("Hello HyperBEAM tools")
app:SetRoot(text, true)
app:Run()