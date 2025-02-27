# Pre-conditions 
1. you’ve searched QUDT for an existing unit, and not found one: https://ble-lter.github.io/QUDT_Browser/
2. A Unit WG member has a clone of EDI’s fork of QUTD (https://github.com/EDIorg/qudt-public-repo/), and confirmed this unit is not staged for release (link here). You will working in this directory: qudt-public-repo/src/main/rdf/vocab/

# Steps
1. Assemble Unit Info
    1. Fields needed (Required Unit Properties)
        - Id, following Qname Naming Rules 
        - Label 
        - plain text description
        - SI offset (conversion from this unit to SI)
        - SI multiplier (conversion to SI)
        - Dimension vector*, per Introducing dimension vectors (more below)
        - Quantity kind* (more below)
        - Symbol (optional, but preferred)
        - UCUM code (optional, but preferred)
     2. Consult with Units WG as needed
        - Please include at least one dataset id or draft (for picking a quantity kind)
2. Look for an already-defined Dimension vector and Quantity kind. Most likely there is a DV, and if so, there may also be an appropriate QK. If not, you will define one or both (see flowchart).
   1. Open QK file, search by DV
        - quantitykinds/VOCAB_QUDT-QUANTITY-KINDS-ALL.ttl
        - Use editor’s find-all command
   2. If no DV found, you will add both a DV and a QK
        1. Define new DV
            - Where: dimensionvectors/VOCAB_QUDT-DIMENSION-VECTORS.ttl 
            - Required fields:
                - A
                - B
                - C
                - D
        2. Define new QK (see below)
            3.     Where: QK file is probably still open.
             - Required fields:
                - A
                - B
                - C
                - D
                - No no" applicableunits! This is pop’d by code
    3. If the DV is found, try to find an appropriate QK within that group
            - If none: Define new QK in QK vocab, See b.ii 
                - quantitykinds/VOCAB_QUDT-QUANTITY-KINDS-ALL.ttl
3. Finish Unit definition
    - Add unit ttl to unit/VOCAB_QUDT-UNITS-ALL.ttl
4. Commit 1-3 files
    - Locally
5. Push to our fork (Validates)
    - How to find validation results… coming soon
6. Serialize 
    - Github action > “Format sources and commit to branch” 
