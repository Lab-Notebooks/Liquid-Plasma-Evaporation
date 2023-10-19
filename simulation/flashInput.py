"""Script to create input files"""

import os
import toml


def createParfile(inputDict):
    """
    Create flash.par
    """
    with open(os.getenv("JobWorkDir") + os.sep + "flash.par", "w") as parfile:
        # Add comment to the parfile
        parfile.write("# Programmatically generated parfile for flashx\n")

        # Loop over keys
        for key, value in inputDict["Parfile"].items():
            if type(value) == str and value not in [
                ".TRUE.",
                ".FALSE.",
                ".true.",
                ".false.",
            ]:
                parfile.write(f'{key} = "{value}"\n')
            else:
                parfile.write(f"{key} = {value}\n")

    print(f"Wrote parameter information to file flash.par")


if __name__ == "__main__":
    # Load toml dictionary
    inputDict = toml.load(os.getenv("JobWorkDir") + os.sep + "job.input")

    # Create input files
    createParfile(inputDict)
