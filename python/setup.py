from setuptools import setup, find_packages

setup(
    name="csbc-synapse-utility-scripts",
    packages=find_packages(),
    license='Apache License, Version 2.0',
    version='1.0.0',
    install_requires=[
        'pandas',
        'synapseclient'],
    entry_points={
        'console_scripts': ['csbc = csbc.__main__:main']
    },
    author="Nasim Sanati",
    author_email="nasim.sanati@sagebase.org",
)
