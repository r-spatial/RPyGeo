import pkgutil
import re

module = {}

# Get functions from main module
module["main"] = dir(arcpy)

# Get functions from other modules and extensions
for importer, modname, ispkg in pkgutil.iter_modules(arcpy.__path__):
  if(re.match('[A-Za-z].*', modname)):
    try:
      module[modname] = eval('dir(arcpy.' + modname + ')')
    except:
      pass






