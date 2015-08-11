#EntrezHTTP
[![Hackage](https://img.shields.io/hackage/v/EntrezHTTP.svg)](https://hackage.haskell.org/package/EntrezHTTP) [![Build Status](https://travis-ci.org/eggzilla/EntrezHTTP.svg?branch=master)](https://travis-ci.org/eggzilla/EntrezHTTP)
=========

EntrezHTTP is a haskell cabal libary, providing libary functions to interface for the [NCBI Entrez REST](http://www.ncbi.nlm.nih.gov/Class/MLACourse/Original8Hour/Entrez/) webservice.

The included function provide either the raw output of the query, enabling custom postprocessing,
or in case of Taxonomy (via [Taxonomy](https://github.com/eggzilla/Taxonomy) library) related queries parsed output.
