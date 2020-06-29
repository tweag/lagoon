from setuptools import setup

setup(name='PyDatalake',
      version='4.0.0',
      description='Python interface to Pfizer Datalake',
      url='http://github.com/tweag/datalake',
      author='Yves Pares',
      author_email='yves.pares@tweag.io',
      license='AllRightsReserved',
      packages=['PyDatalake'],
      install_requires=[
          'sqlalchemy>=1.1.3', 'psycopg2', 'PyYAML', 'requests', 'pandas'
      ],
      zip_safe=False)
