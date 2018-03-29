from setuptools import setup, find_packages

setup(
    name="synapse-dcc-utility-scripts",
    packages=find_packages(),
    license='Apache License, Version 2.0',
    version='1.0.0',
    install_requires=[
        'pandas',
        'synapseclient'],
    entry_points={
        'console_scripts': ['syndcc = syndcc.__main__:main']
    },
    author="Nasim Sanati",
    author_email="nasim.sanati@sagebionetworks.org",
)
