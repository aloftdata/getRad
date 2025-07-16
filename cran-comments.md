# Resubmission

With the detailed feedback by Benjamin Altmann (for reference copied below) I have updated the package. The following steps have been taken:

1: Currently there is no reference describing the code in more detail. The most elaborate description is the package website that is referenced in the DESCRIPTION file. I will include a reference in the future if a description is published however no changes were made now for this issue.
2. I have removed the examples for these none exported functions
3. Thank you for spotting this omission. I have consulted with the original author (Hidde Leijnse) and Bart Hoekstra who has modified this code. This source file is covered by the same license as the package. I have added both as contributors to the package after consultation.

I hope these changes sufficiently adress your concerns

Thank you very much in advance

Bart


If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")
For more details: <https://eur04.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcontributor.r-project.org%2Fcran-cookbook%2Fdescription_issues.html%23references&data=05%7C02%7Cb.kranstauber%40uva.nl%7C4d4db10c04464f5a65a408ddc0798891%7Ca0f1cacd618c4403b94576fb3d6874e5%7C0%7C0%7C638878352884584613%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C40000%7C%7C%7C&sdata=kNLSK8I57ybjG0HokQsT0AvoAPrbscCasAkn%2FKqRHwQ%3D&reserved=0>

You have examples for unexported functions. Please either omit these examples or export these functions.
Examples for unexported function
  get_vpts_aloft() in:
     get_vpts_aloft.Rd
  get_vpts_rmi() in:
     get_vpts_rmi.Rd

Please always add all authors, contributors and copyright holders in the Authors@R field with the appropriate roles.
From CRAN policies you agreed to:
"The ownership of copyright and intellectual property rights of all components of the package must be clear and unambiguous (including from the authors specification in the DESCRIPTION file). Where code is copied (or derived) from the work of others (including from R itself), care must be taken that any copyright/license statements are preserved and authorship is not misrepresented.
Preferably, an ‘Authors@R’ would be used with ‘ctb’ roles for the authors of such code. Alternatively, the ‘Author’ field should list these authors as contributors. Where copyrights are held by an entity other than the package authors, this should preferably be indicated via ‘cph’ roles in the ‘Authors@R’ field, or using a ‘Copyright’ field (if necessary referring to an inst/COPYRIGHTS file)."
e.g.: -> "Hidde Leijnse" in KNMI_vol_h5_to_ODIM_h5.c
Please explain in the submission comments what you did about this issue.
For more details: <https://eur04.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcontributor.r-project.org%2Fcran-cookbook%2Fdescription_issues.html%23using-authorsr&data=05%7C02%7Cb.kranstauber%40uva.nl%7C4d4db10c04464f5a65a408ddc0798891%7Ca0f1cacd618c4403b94576fb3d6874e5%7C0%7C0%7C638878352884612938%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C40000%7C%7C%7C&sdata=9imPQrBX7%2BlwrwXsDLI8jwJS46TGalC%2BItnEC8UBJo8%3D&reserved=0>




# Check notes

Note the package checks identify "aeroecological" as a misspelled word, however aeroecology is a discipline of ecology (https://en.wikipedia.org/wiki/Aeroecology).
