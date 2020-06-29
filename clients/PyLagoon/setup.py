from setuptools import setup

setup(name='PyLagoon',
      version='4.0.0',
      description='Python interface to Lagoon',
      url='http://github.com/tweag/lagoon',
      author='Yves Pares',
      author_email='yves.pares@tweag.io',
      license='AllRightsReserved',
      packages=['PyLagoon'],
      install_requires=[
          'sqlalchemy>=1.1.3', 'psycopg2', 'PyYAML', 'requests', 'pandas'
      ],
      zip_safe=False)
