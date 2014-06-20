module Plc where

{--

  _____ _      _                     _             _       _                                   _          _____                      _ _           
 |  __ (_)    | |                   | |           ( )     | |                                 | |        / ____|                    (_) |          
 | |__) |  ___| |_ ___ _ ____   ____| |_   ___ __ |/ ___  | |     __ _ _ __   __ _ _   _  __ _| |_ ___  | |     ___  _ __ ___  _ __  _| | ___ _ __ 
 |  ___/ |/ _ \ __/ _ \ '__\ \ / / _` \ \ / / '_ \  / __| | |    / _` | '_ \ / _` | | | |/ _` | __/ _ \ | |    / _ \| '_ ` _ \| '_ \| | |/ _ \ '__|
 | |   | |  __/ ||  __/ |   \ V / (_| |\ V /| | | | \__ \ | |___| (_| | | | | (_| | |_| | (_| | ||  __/ | |___| (_) | | | | | | |_) | | |  __/ |   
 |_|   |_|\___|\__\___|_|    \_/ \__,_| \_/ |_| |_| |___/ |______\__,_|_| |_|\__, |\__,_|\__,_|\__\___|  \_____\___/|_| |_| |_| .__/|_|_|\___|_|   
                                                                              __/ |                                           | |                  
                                                                             |___/                                            |_|                  

The frontend/executable of the languate compiler



TODO

- Finish parser

- Module retrieval from disk using imports

- Semantic analysis
	- building of symbol tables
	- type check+infer

- Interpreter

- Lawcheck (dirty)

- Manifest parser
- Docgen

- Cross-project parsing


--}
