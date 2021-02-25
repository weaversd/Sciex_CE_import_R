#Creating a custom overlay:

df1 <- create_sciex_table('2021_02_18_UV_humanserum_0.5mgmL_inj2psi7sec_sep1.5kV_1MAceticAcid_90mins.dat.asc.xlsx',
                       name = "HS 0.5mg/mL inj_1")

df2 <- create_sciex_table('2021_02_18_UV_humanserum_0.5mgmL_inj2psi7sec_sep1.5kV_1MAceticAcid_75mins_2.asc.xlsx',
                          name = "HS 0.5mg/mL inj_2")

df3 <- create_sciex_table('2021_02_18_UV_humanserum_0.5mgmL_inj2psi7sec_sep1.5kV_1MAceticAcid_75mins_3.dat.asc.xlsx',
                          name = "HS 0.5mg/mL inj_3")

combined_CE_df <- combine_CE_traces(df1, df2, df3)

overlay_plot <- plot_sciex_overlay(combined_CE_df, ylab = 'Abs @ 214nm')

overlay_plot
save_as_tif(overlay_plot,
            "2021_02_18_UV_Humanserum_0.5mgmL_20psi7sec_1.5kV_1MAceticAcid_70mins.tiff",
            w = 15, h = 6)

combined_CE_df_23 <- combine_CE_traces(df2, df3)

overlay_23 <- plot_sciex_overlay(combined_CE_df_23, ylab = 'Abs @ 214nm')

overlay_23

#creating fractions
plot4 + geom_vline(xintercept = 20, color = 'red')
+ geom_vline(xintercept = 26.25, color = "red")
+ geom_vline(xintercept = 32.5, color = 'red')
+ geom_vline(xintercept = 38.7, color = 'red')
+ geom_vline(xintercept = 45, color = 'red')

#create overlay with fractions
df4 <- create_sciex_table('20210223_UV_HumanSerum_0.5mgmL_AceticAcid_Inj2psi_7s_Sep1.5kV_75min.dat.asc.xlsx',
                          name = 'HS test inj')
df5 <- create_sciex_table('2021_02_23_UV_HumanSerum_plug_1.dat.asc.xlsx', name = 'HS plug collection')
df6 <- create_sciex_table('2021_02_23_UV_HumanSerum_4Fraction_1.dat.asc.xlsx', name = 'HS 4 frac collection')
combined_456_df <- combine_CE_traces(df4, df5, df6)
overlay_456 <- plot_sciex_overlay(combined_456_df, ylab = 'Abs @ 214nm', xmin = 20, xmax = 65, ymin = -0.003)
overlay_456 + geom_vline(xintercept = 20, color = 'orange') + 
  geom_vline(xintercept = 26.25, color = "orange") +
  geom_vline(xintercept = 32.5, color = 'orange') +
  geom_vline(xintercept = 38.7, color = 'orange') +
  geom_vline(xintercept = 45, color = 'orange')

combined_56_df <- combine_CE_traces(df5, df6)
overlay_56 <- plot_sciex_overlay(combined_56_df, ylab ='Abs @214nm', xmin = 20, xmax = 65)
overlay_56 + geom_vline(xintercept = 20, color = 'orange') + 
  geom_vline(xintercept = 26.25, color = "orange") +
  geom_vline(xintercept = 32.5, color = 'orange') +
  geom_vline(xintercept = 38.7, color = 'orange') +
  geom_vline(xintercept = 45, color = 'orange')


#create plug overlay
dfplug1 <- create_sciex_table('2021_02_23_UV_HumanSerum_plug_1.dat.asc.xlsx',
                              name = "HS plug 1")
dfplug2 <- create_sciex_table('2021_02_23_UV_HumanSerum_plug_2.dat.asc.xlsx',
                              name = "HS plug 2")
dfplug3 <- create_sciex_table('2021_02_23_UV_HumanSerum_plug_3.dat.asc.xlsx',
                              name = "HS plug 3")
dfplug4 <- create_sciex_table('2021_02_23_UV_HumanSerum_plug_4.dat.asc.xlsx',
                              name = "HS plug 4")
dfplug5 <- create_sciex_table('2021_02_23_UV_HumanSerum_plug_5.dat.asc.xlsx',
                              name = "HS plug 5")
dfplug6 <- create_sciex_table('2021_02_23_UV_HumanSerum_plug_6.dat.asc.xlsx',
                              name = "HS plug 6")

combined_plug_df <- combine_CE_traces(dfplug1, dfplug2, dfplug3, dfplug4, dfplug5, dfplug6)
overlay_plug <- plot_sciex_overlay(combined_plug_df, ylab = 'Abs @ 214nm', xmin = 20, xmax = 62)
add_fractions(overlay_plug, 20, 26.25, 32.5, 38.7, 45, 'black')
add_fractions(overlay_plug, 0, 0, 0, 20, 45, 'black')

#create fraction collection overlay
dffrac1 <- create_sciex_table('2021_02_23_UV_HumanSerum_4Fraction_1.dat.asc.xlsx',
                              name = "HS 4Frac 1")
dffrac2 <- create_sciex_table('2021_02_23_UV_HumanSerum_4Fraction_2.dat.asc.xlsx',
                              name = "HS 4Frac 2")
dffrac3 <- create_sciex_table('2021_02_23_UV_HumanSerum_4Fraction_3.dat.asc.xlsx',
                              name = "HS 4Frac 3")
dffrac4 <- create_sciex_table('2021_02_23_UV_HumanSerum_4Fraction_4.dat.asc.xlsx',
                              name = "HS 4Frac 4")
dffrac5 <- create_sciex_table('2021_02_23_UV_HumanSerum_4Fraction_5.dat.asc.xlsx',
                              name = "HS 4Frac 5")
dffrac6 <- create_sciex_table('2021_02_23_UV_HumanSerum_4Fraction_6.dat.asc.xlsx',
                              name = "HS 4Frac 6")

combined_frac_df <- combine_CE_traces(dffrac1, dffrac2, dffrac3, dffrac4, dffrac5, dffrac6)
overlay_frac <- plot_sciex_overlay(combined_frac_df, ylab = 'Abs @ 214nm', xmin = 20, xmax = 62)
add_fractions(overlay_frac, 20, 26.25, 32.5, 38.7, 45, 'black')
