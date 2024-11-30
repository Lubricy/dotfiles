sort=-time
layer=overlay
width=300
height=110
border-size=2
border-radius=10
icons=0
max-icon-size=64
margin=15

[urgency=low]
default-timeout=5000
background-color={{colors.secondary.default.hex}}
text-color={{colors.on_secondary.default.hex}}
border-color={{colors.secondary_container.default.hex}}

[urgency=normal]
default-timeout=30000
background-color={{colors.primary.default.hex}}
text-color={{colors.on_primary.default.hex}}
border-color={{colors.primary_container.default.hex}}

[urgency=high]
ignore-timeout=1
default-timeout=0
background-color={{colors.error.default.hex}}
text-color={{colors.on_error.default.hex}}
border-color={{colors.error_container.default.hex}}

[category=mpd]
default-timeout=2000
group-by=category
