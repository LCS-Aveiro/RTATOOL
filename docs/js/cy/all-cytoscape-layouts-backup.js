window.RTA_DEFAULT_LAYOUTS  = 
{
  "cyLayout_1570462910": {
    "nodes": {
      "event_z_z_zz_zz": {
        "x": 268.11230366492146,
        "y": 198.0193717277487
      },
      "event_z_y_zy_zy": {
        "x": 413.0264397905759,
        "y": 18.996335078534024
      },
      "x": {
        "x": 102.31125654450261,
        "y": -85.13664921465968
      },
      "y": {
        "x": 446.2196335078535,
        "y": -79.84031413612567
      },
      "event_x_z_xz_xz": {
        "x": 136.10445026178013,
        "y": 39.3712041884817
      },
      "event_x_x_xx_xx": {
        "x": 52.83795811518327,
        "y": -154.43979057591625
      },
      "z": {
        "x": 273.55,
        "y": 80.6
      }
    },
    "edges": {}
  },
  "cyLayout_name_Conflict": {
    "nodes": {
      "i3": {
        "x": 816.25,
        "y": 34.800000000000004
      },
      "i2": {
        "x": 570.25,
        "y": 34.800000000000004
      },
      "i1": {
        "x": 257.27967695135305,
        "y": 45.8032162698287
      },
      "event_i0_i1_a_a": {
        "x": 78.85000000000002,
        "y": 59.7
      },
      "event_a_b_on_on": {
        "x": 194.8281882008147,
        "y": 168.41490172202444
      },
      "event_i1_i2_b_b": {
        "x": 447.25,
        "y": 34.800000000000004
      },
      "event_on_b_off_off": {
        "x": 402.8516890140765,
        "y": 160.12817326673087
      },
      "event_i2_i3_c_c": {
        "x": 693.25,
        "y": 34.800000000000004
      },
      "i0": {
        "x": -44.75,
        "y": 59.7
      }
    },
    "edges": {}
  },
  "cyLayout_name_Vending3prod": {
    "nodes": {
      "event_soda_pay_getSoda_getSoda": {
        "x": 136.67096774193547,
        "y": 35.841935483871026
      },
      "soda": {
        "x": 319.717269138803,
        "y": -33.46256953770409
      },
      "event_select_soda_askSoda_askSoda": {
        "x": 444.28599832061326,
        "y": 35.02778081158402
      },
      "event_askSoda_askSoda_noSoda_noSoda": {
        "x": 510.897344114139,
        "y": -62.22940793083616
      },
      "beer": {
        "x": 376.0925472564486,
        "y": 345.9113320418861
      },
      "event_select_beer_askBeer_askBeer": {
        "x": 457.6,
        "y": 220.7
      },
      "pay": {
        "x": 88,
        "y": 269.3
      },
      "event_askBeer_askBeer_noBeer_noBeer": {
        "x": 528.2593437312526,
        "y": 252.18157673764892
      },
      "event_pay_select_insertCoin_insertCoin": {
        "x": 230.13548387096776,
        "y": 188.88064516129032
      },
      "event_askSoda_noSoda_askSodataunoSoda_askSodataunoSoda": {
        "x": 565.183787529131,
        "y": 35.02778081158402
      },
      "event_beer_pay_getBeer_getBeer": {
        "x": 275.10322580645163,
        "y": 351.416129032258
      },
      "select": {
        "x": 334.6,
        "y": 152.9
      }
    },
    "edges": {
      "s_to_a_pay_event_pay_select_insertCoin_insertCoin": {
        "distances": [
          -5.017121813556654
        ],
        "weights": [
          0.6134513093403683
        ]
      },
      "s_to_a_beer_event_beer_pay_getBeer_getBeer": {
        "distances": [
          -22.888259606621087
        ],
        "weights": [
          0.4204436739103095
        ]
      },
      "a_to_s_event_pay_select_insertCoin_insertCoin_select": {
        "distances": [
          4.14786040691067
        ],
        "weights": [
          0.36639272290098923
        ]
      },
      "a_to_s_event_select_beer_askBeer_askBeer_beer": {
        "distances": [
          -18.218017757890497
        ],
        "weights": [
          0.4741770299366801
        ]
      },
      "a_to_s_event_beer_pay_getBeer_getBeer_pay": {
        "distances": [
          -19.24415512575132
        ],
        "weights": [
          0.4822533499770665
        ]
      }
    }
  },
  "cyLayout_name_simple": {
    "nodes": {
      "a": {
        "x": -87,
        "y": 45
      },
      "event_a_b_ab_ab": {
        "x": 15,
        "y": 103
      },
      "b": {
        "x": 101,
        "y": 39
      },
      "event_b_a_ba_ba": {
        "x": 13,
        "y": -13
      },
      "event_ba_ab_off_off": {
        "x": 15,
        "y": 40
      }
    },
    "edges": {
      "rule_from_event_b_a_ba_ba_event_ba_ab_off_off": {
        "distances": [
          0.43774194175750847
        ],
        "weights": [
          0.5203639262539197
        ]
      },
      "rule_to_event_ba_ab_off_off_event_a_b_ab_ab": {
        "distances": [
          -0.2790902341293969
        ],
        "weights": [
          0.4080451438604504
        ]
      }
    }
  },
  "cyLayout_name_SmartHeater": {
    "nodes": {
      "event_On_Off_turnOff_turnOff": {
        "x": 297.670622789318,
        "y": 188.10532245986
      },
      "event_Off_Off_repair_repair": {
        "x": -21.971548120300064,
        "y": 3.673032535991812
      },
      "event_Off_On_turnOn_turnOn": {
        "x": 274.4892528108076,
        "y": -37.45318114124589
      },
      "event_On_Off_overheat_overheat": {
        "x": 286.1165627749105,
        "y": 86.26754803957341
      },
      "event_overheat_turnOn_overheattauturnOn_overheattauturnOn": {
        "x": 278.43180531984,
        "y": 26.706234999333358
      },
      "On": {
        "x": 535.3888560222013,
        "y": 68.26350339223497
      },
      "event_repair_repair_repairtaurepair_repairtaurepair": {
        "x": -91.6615959552621,
        "y": -6.381506936346388
      },
      "event_overheat_repair_overheattaurepair_overheattaurepair": {
        "x": 111.23679009571615,
        "y": 46.956010702456965
      },
      "Off": {
        "x": 21.684513834511947,
        "y": 90.24320669614777
      },
      "event_repair_turnOn_repairtauturnOn_repairtauturnOn": {
        "x": 109.78520660836251,
        "y": -66.56974199868185
      },
      "note_749315792": {
        "x": 688.5840678792363,
        "y": 23.19758177854714
      },
      "note_-1119831568": {
        "x": 417.347992210048,
        "y": -133.37784215417412
      },
      "note_892647597": {
        "x": 353.5494734273405,
        "y": 255.32750994503272
      },
      "note_1447849665": {
        "x": 57.8365945574197,
        "y": 214.9028234616268
      },
      "note_119014155": {
        "x": 48.04728662646787,
        "y": -127.8963212723738
      }
    },
    "edges": {
      "s_to_a_On_event_On_Off_turnOff_turnOff": {
        "distances": [
          -37.93236716315624
        ],
        "weights": [
          0.47967632239417624
        ]
      },
      "a_to_s_event_On_Off_turnOff_turnOff_Off": {
        "distances": [
          -36.33767454111063
        ],
        "weights": [
          0.5281189760965487
        ]
      },
      "rule_from_event_On_Off_overheat_overheat_event_overheat_turnOn_overheattauturnOn_overheattauturnOn": {
        "distances": [
          -0.3113210075082092
        ],
        "weights": [
          0.6009243398803839
        ]
      },
      "s_to_a_Off_event_Off_On_turnOn_turnOn": {
        "distances": [
          -50.80088138241507
        ],
        "weights": [
          0.4662819428890696
        ]
      },
      "rule_from_event_On_Off_overheat_overheat_event_overheat_repair_overheattaurepair_overheattaurepair": {
        "distances": [
          -9.900105877038147
        ],
        "weights": [
          0.6549896771943472
        ]
      },
      "rule_to_event_overheat_repair_overheattaurepair_overheattaurepair_event_Off_Off_repair_repair": {
        "distances": [
          19.06175792478369
        ],
        "weights": [
          0.3953375315781965
        ]
      },
      "a_to_s_event_On_Off_overheat_overheat_Off": {
        "distances": [
          0.40367917044307094
        ],
        "weights": [
          0.5214805709582563
        ]
      },
      "rule_to_event_overheat_turnOn_overheattauturnOn_overheattauturnOn_event_Off_On_turnOn_turnOn": {
        "distances": [
          0.5234454974163972
        ],
        "weights": [
          0.3440440066328491
        ]
      },
      "s_to_a_On_event_On_Off_overheat_overheat": {
        "distances": [
          3.4918947968586953
        ],
        "weights": [
          0.5625413015332671
        ]
      },
      "a_to_s_event_Off_On_turnOn_turnOn_On": {
        "distances": [
          -23.85101788264956
        ],
        "weights": [
          0.4525814344304762
        ]
      },
      "rule_to_event_repair_turnOn_repairtauturnOn_repairtauturnOn_event_Off_On_turnOn_turnOn": {
        "distances": [
          -33.47320570138917
        ],
        "weights": [
          0.4344771041185258
        ]
      },
      "rule_from_event_Off_Off_repair_repair_event_repair_turnOn_repairtauturnOn_repairtauturnOn": {
        "distances": [
          -29.60526558602865
        ],
        "weights": [
          0.5176693251114912
        ]
      }
    }
  },
  "cyLayout_name_GRG": {
    "nodes": {
      "s0": {
        "x": -29.708064516129042,
        "y": 240.88037634408593
      },
      "event_s1_s2_cc_cc": {
        "x": 450.7572580645161,
        "y": -145.58521505376345
      },
      "event_aa_aa_offA2_offA2": {
        "x": 114.64413978494629,
        "y": -6.5022043010752135
      },
      "event_bb_offA2_onOffA_onOffA": {
        "x": 195.14337365591402,
        "y": 69.49564516129033
      },
      "event_aa_bb_onB_onB": {
        "x": 126.42322580645165,
        "y": 118.18130376344084
      },
      "s2": {
        "x": 546.7169354838707,
        "y": -141.97983870967738
      },
      "s1": {
        "x": 306.12500000000006,
        "y": -150.56666666666663
      },
      "event_s0_s1_aa_aa": {
        "x": -7.959946236559117,
        "y": -127.7142473118279
      },
      "event_s1_s0_bb_bb": {
        "x": 356.1161290322581,
        "y": 233.05080645161289
      }
    },
    "edges": {}
  },
  "cyLayout_name_Tank_Control_System": {
    "nodes": {
      "P2": {
        "x": 1506.8179076827123,
        "y": 34.69576547761574
      },
      "event_wait_drain_close_valve_wait_draintauclose_valve_wait_draintauclose_valve": {
        "x": 1101.5387432009204,
        "y": 397.3716012085739
      },
      "event_level_high_activate_P2_level_hightauactivate_P2_level_hightauactivate_P2": {
        "x": 1745.9900310649127,
        "y": 194.73617151510823
      },
      "ST2": {
        "x": 1262.1094841384995,
        "y": 279.14190592781114
      },
      "ST3": {
        "x": 1693.5007499451594,
        "y": 384.9900310649128
      },
      "event_ST1_ST2_level_low_level_low": {
        "x": 926.8,
        "y": 248
      },
      "event_T_V_close_valve_close_valve": {
        "x": 657.3520303035092,
        "y": 450.4739046778743
      },
      "event_close_valve_filling_close_valvetaufilling_close_valvetaufilling": {
        "x": 619.8101682074572,
        "y": 304.9390749831769
      },
      "event_ST3_T_timeout_timeout": {
        "x": 1729.6558442367107,
        "y": 546.1633436883042
      },
      "ST1": {
        "x": 778.6,
        "y": 185.60000000000002
      },
      "START": {
        "x": 546.5010371551278,
        "y": 109.16208128785183
      },
      "event_START_ST1_filling_filling": {
        "x": 636.4000000000001,
        "y": 185.60000000000002
      },
      "event_ST3_ST3_wait_drain_wait_drain": {
        "x": 1558.375032161726,
        "y": 437.81596849636196
      },
      "V": {
        "x": 463.2699691372806,
        "y": 440.2935801013433
      },
      "P1": {
        "x": 875.7730019742638,
        "y": -115.522718002796
      },
      "event_ST2_P2_activate_P2_activate_P2": {
        "x": 1489.310188583103,
        "y": 135.53043710240522
      },
      "event_ST1_P1_activate_P1_activate_P1": {
        "x": 890.7258919308931,
        "y": 37.2099689350872
      },
      "T": {
        "x": 1332.7745018645824,
        "y": 554.2610938528263
      },
      "event_ST2_ST3_level_high_level_high": {
        "x": 1696.0219680576365,
        "y": 291.02518748628984
      },
      "event_level_low_activate_P1_level_lowtauactivate_P1_level_lowtauactivate_P1": {
        "x": 1041.1019846869062,
        "y": 135.37031284275415
      }
    },
    "edges": {}
  },
  "cyLayout_name_EX": {
    "nodes": {
      "event_w12_w21_onW_onW": {
        "x": 144.36862685510403,
        "y": 82.57381807487074
      },
      "w2": {
        "x": 262.0999999999999,
        "y": -3.9499999999999886
      },
      "w1": {
        "x": 66.1,
        "y": 276.05
      },
      "w3": {
        "x": 328.7529273815157,
        "y": 554.4314006145847
      },
      "event_w3_w1_w31_w31": {
        "x": 355.5,
        "y": 321.85
      },
      "event_w3_w3_tau_deadlock": {
        "x": 467.5,
        "y": 609.05
      },
      "event_w2_w2_tau_deadlock": {
        "x": 348.70000000000005,
        "y": -58.94999999999999
      },
      "event_w13_w31_onW2_onW2": {
        "x": 271.1271988537296,
        "y": 430.09511472715633
      },
      "event_w1_w2_w12_w12": {
        "x": 250.5,
        "y": 125.25
      },
      "event_w1_w3_w13_w13": {
        "x": 49.30000000000001,
        "y": 438.05
      },
      "event_w13_w12_offW_offW": {
        "x": 239.89999999999998,
        "y": 346.65
      },
      "event_w2_w1_w21_w21": {
        "x": 41.97533898707182,
        "y": 47.157102758174474
      }
    },
    "edges": {
      "rule_from_event_w1_w2_w12_w12_event_w12_w21_onW_onW": {
        "distances": [
          2.180224997870219
        ],
        "weights": [
          0.47786601703812837
        ]
      },
      "rule_to_event_w12_w21_onW_onW_event_w2_w1_w21_w21": {
        "distances": [
          6.5915061613089385
        ],
        "weights": [
          0.5624281723073522
        ]
      }
    }
  },
  "cyLayout_name_SecureLogin": {
    "nodes": {
      "Blocked": {
        "x": 178.8499999999999,
        "y": 124.07499999999999
      },
      "event_Idle_Idle_retry_retry": {
        "x": -161.34999999999997,
        "y": 110.075
      },
      "Idle": {
        "x": -42.75,
        "y": 185.875
      },
      "event_Idle_Active_login_login": {
        "x": 55.849999999999994,
        "y": 288.975
      },
      "Active": {
        "x": 222.85000000000002,
        "y": 318.975
      },
      "event_Active_Blocked_timeout_timeout": {
        "x": 293.85,
        "y": 206.27499999999998
      },
      "event_retry_block_enableBlock_enableBlock": {
        "x": -67.81929553470297,
        "y": 21.409365342379374
      },
      "event_Idle_Blocked_block_block": {
        "x": 63.705211552540206,
        "y": 89.5355616556777
      },
      "event_Active_Idle_logout_logout": {
        "x": 152.85000000000002,
        "y": 214.875
      }
    },
    "edges": {
      "s_to_a_Idle_event_Idle_Blocked_block_block": {
        "distances": [
          -75.12227443139433
        ],
        "weights": [
          0.5320779343812438
        ]
      },
      "a_to_s_event_Idle_Blocked_block_block_Blocked": {
        "distances": [
          -45.987409911010516
        ],
        "weights": [
          0.5409624273881732
        ]
      },
      "rule_to_event_retry_block_enableBlock_enableBlock_event_Idle_Blocked_block_block": {
        "distances": [
          -74.3665772622463
        ],
        "weights": [
          0.38957197445654823
        ]
      },
      "rule_from_event_Idle_Idle_retry_retry_event_retry_block_enableBlock_enableBlock": {
        "distances": [
          -52.74660675386596
        ],
        "weights": [
          0.4674342135771243
        ]
      }
    }
  },
  "cyLayout_-1374786202": {
    "nodes": {
      "event_x_z_xz_xz": {
        "x": 68.83376963350784,
        "y": 262.282722513089
      },
      "event_yx_xz_yxxz_yxxz": {
        "x": 229.5541884816754,
        "y": -43.870157068062824
      },
      "event_z_v_vz_vz": {
        "x": 696.452617801047,
        "y": 382.6910994764397
      },
      "event_x_x_xx_xx": {
        "x": -6.214397905759167,
        "y": -199.02722513089
      },
      "event_z_y_zy_zy": {
        "x": 624.6803664921466,
        "y": 206.38062827225133
      },
      "event_y_y_yy_yy": {
        "x": 899.7337696335078,
        "y": -163.0586387434555
      },
      "event_xy_zy_xyyz_xyyz": {
        "x": 456.85209424083774,
        "y": 191.56544502617805
      },
      "v": {
        "x": 806.2777486910995,
        "y": 328.54712041884807
      },
      "event_y_x_yx_yx": {
        "x": 414.95628272251304,
        "y": -199.4314136125654
      },
      "x": {
        "x": 55.78979057591624,
        "y": -104.53979057591621
      },
      "y": {
        "x": 764.7253926701571,
        "y": -103.14345549738218
      },
      "event_x_y_xy_xy": {
        "x": 491.15471204188475,
        "y": 1.322513089005323
      },
      "event_z_z_zz_zz": {
        "x": 406.8452879581152,
        "y": 485.7717277486911
      },
      "z": {
        "x": 527.4468586387435,
        "y": 371.8890052356021
      }
    },
    "edges": {}
  },
  "cyLayout_name_AdvancedBot": {
    "nodes": {
      "event_Home_Home_battery_low_battery_low": {
        "x": 58.873527070054976,
        "y": 426.6195850825112
      },
      "event_battery_low_go_charge_battery_lowtaugo_charge_battery_lowtaugo_charge": {
        "x": 516.6584232049825,
        "y": 340.7020745874439
      },
      "event_Office_Office_high_stress_high_stress": {
        "x": 783.8054773450922,
        "y": 625.8120331499749
      },
      "event_Station_Home_finish_charge_finish_charge": {
        "x": 175.57518728281067,
        "y": 181.85601657498756
      },
      "event_high_stress_easy_task_high_stresstaueasy_task_high_stresstaueasy_task": {
        "x": 923.3460168463879,
        "y": 493.73917016502236
      },
      "event_finish_charge_socialize_finish_chargetausocialize_finish_chargetausocialize": {
        "x": 122.2198347929257,
        "y": 38.650166021275574
      },
      "Home": {
        "x": 19.78178425376192,
        "y": 321.9670124312406
      },
      "event_battery_low_go_work_battery_lowtaugo_work_battery_lowtaugo_work": {
        "x": 414.12817430876936,
        "y": 618.8646058012457
      },
      "Station": {
        "x": 318.641535628949,
        "y": 157.53058093876433
      },
      "event_Home_Office_go_work_go_work": {
        "x": 463.94327817384163,
        "y": 510.4109958562532
      },
      "event_Office_Home_go_home_go_home": {
        "x": 464.05012485520706,
        "y": 427.86601630358723
      },
      "event_no_money_go_work_no_moneytaugo_work_no_moneytaugo_work": {
        "x": -18.584232049825317,
        "y": 524.2580911624315
      },
      "event_Home_Home_no_money_no_money": {
        "x": -88.83954337364202,
        "y": 371.3316182324863
      },
      "Office": {
        "x": 645.3480914338318,
        "y": 501.7219917125062
      },
      "event_Home_Home_socialize_socialize": {
        "x": -6.109958562531187,
        "y": 192.28659751375187
      },
      "event_Office_Office_easy_task_easy_task": {
        "x": 750.6604154602895,
        "y": 402.82817430876935
      },
      "event_Home_Station_go_charge_go_charge": {
        "x": 265.83398369641276,
        "y": 258.3230290062281
      }
    },
    "edges": {}
  },
  "cyLayout_name_DynamicSPL": {
    "nodes": {
      "sent_encrypt": {
        "x": 26.5,
        "y": 172.9085924460899
      },
      "event_Safe_ERoute_SafetauERoute_SafetauERoute": {
        "x": 522.3950454544195,
        "y": 263.60664249992584
      },
      "received": {
        "x": 382.59417779905925,
        "y": 163.3393960882876
      },
      "event_Encrypt_ESend_EncrypttauESend_EncrypttauESend": {
        "x": 167.31467610454885,
        "y": 325.84505482918456
      },
      "routed_safe": {
        "x": 343.71722043639693,
        "y": 450.2109085275556
      },
      "event_Encrypt_Send_EncrypttauSend_EncrypttauSend": {
        "x": 167.8633553345982,
        "y": 223.43624829651696
      },
      "event_routed_unsafe_sent_encrypt_ESend_ESend": {
        "x": 104.94798884502191,
        "y": 278.6440104910866
      },
      "sent": {
        "x": 82.86283906115338,
        "y": 349.98868994855246
      },
      "event_routed_safe_sent_ESend_ESend": {
        "x": 195.7197571689684,
        "y": 470.1502540655103
      },
      "ready": {
        "x": 210.25357481905948,
        "y": 72.84159420230273
      },
      "event_setup_setup_Unsafe_Unsafe": {
        "x": 428.81697864465025,
        "y": 379.3657051421177
      },
      "event_Dencrypt_ESend_DencrypttauESend_DencrypttauESend": {
        "x": 131.13525919828166,
        "y": 430.66240173115375
      },
      "event_Unsafe_Route_UnsafetauRoute_UnsafetauRoute": {
        "x": 429.06317452854717,
        "y": 482.4958126449529
      },
      "routed_unsafe": {
        "x": 255.62989345861064,
        "y": 271.2696540004623
      },
      "event_setup_setup_Dencrypt_Dencrypt": {
        "x": 260.87574478309443,
        "y": 387.4324164711357
      },
      "event_sent_encrypt_ready_Ready_Ready": {
        "x": 107.40832508048729,
        "y": 70.35601496190347
      },
      "event_Unsafe_ERoute_UnsafetauERoute_UnsafetauERoute": {
        "x": 503.67896985567563,
        "y": 369.38457865393895
      },
      "setup": {
        "x": 349.1701262925635,
        "y": 248.64031244221252
      },
      "event_setup_setup_Encrypt_Encrypt": {
        "x": 231.63961773390585,
        "y": 178.78574944515094
      },
      "event_routed_unsafe_sent_Send_Send": {
        "x": 196.97261046253223,
        "y": 366.57149878472467
      },
      "event_setup_setup_Safe_Safe": {
        "x": 466.1349583616103,
        "y": 159.20693776297455
      },
      "event_ready_received_Receive_Receive": {
        "x": 330.12931959729485,
        "y": 30.646814767510257
      },
      "event_setup_ready_setuptauready_setuptauready": {
        "x": 295.4333563911498,
        "y": 177.64187576887826
      },
      "event_Dencrypt_Send_DencrypttauSend_DencrypttauSend": {
        "x": 258.8685613207372,
        "y": 500.3615355656219
      },
      "event_ready_setup_readytausetup_readytausetup": {
        "x": 296.55425875484275,
        "y": 95.84700674409032
      },
      "event_Safe_Route_SafetauRoute_SafetauRoute": {
        "x": 459.03330625986166,
        "y": 263.0756413463872
      },
      "event_sent_ready_Ready_Ready": {
        "x": 111.91400637096044,
        "y": 175.48097927320592
      },
      "event_received_routed_unsafe_Route_Route": {
        "x": 363.816887653726,
        "y": 343.61858462557774
      },
      "event_received_routed_safe_ERoute_ERoute": {
        "x": 428.4319985240228,
        "y": 308.41947123359887
      }
    },
    "edges": {}
  },
  "cyLayout_name_Like2": {
    "nodes": {
      "event_FEED_WATCH_watch_fww": {
        "x": 150.7283869151768,
        "y": -183.46988312965144
      },
      "event_fww_wlf_onLike_onLike": {
        "x": 107.6619502905393,
        "y": -64.93089951350686
      },
      "LIST": {
        "x": 273.64472537376645,
        "y": 130.94613805741946
      },
      "event_LIST_WATCH_wacth_lww": {
        "x": 426.08027535366574,
        "y": 46.29464857475865
      },
      "event_FEED_LIST_seeList_fsl": {
        "x": -16.802667803667656,
        "y": 101.18266898036794
      },
      "event_WATCH_FEED_like_wlf": {
        "x": 195.97677328681155,
        "y": -0.9295220348779765
      },
      "FEED": {
        "x": -41.38126524299857,
        "y": -78.8152175540656
      },
      "WATCH": {
        "x": 445.34041976122614,
        "y": -119.93322983045931
      },
      "event_WATCH_FEED_dontLike_wdf": {
        "x": 203.533390974475,
        "y": -137.91973076862368
      },
      "event_wlf_fsl_onSee_onSee": {
        "x": 90.23278835650511,
        "y": 57.36488932437526
      },
      "event_wdf_fsl_offSee_offSee": {
        "x": 276.4481746364618,
        "y": 65.7028916951196
      },
      "event_wdf_wlf_offLike_offLike": {
        "x": 183.5821820828582,
        "y": -68.22681108945648
      }
    },
    "edges": {
      "rule_from_event_WATCH_FEED_dontLike_wdf_event_wdf_fsl_offSee_offSee": {
        "distances": [
          -58.48004175417416
        ],
        "weights": [
          0.5113302244581996
        ]
      },
      "a_to_s_event_FEED_WATCH_watch_fww_WATCH": {
        "distances": [
          -40.82001832310994
        ],
        "weights": [
          0.444735795665673
        ]
      },
      "rule_to_event_wdf_wlf_offLike_offLike_event_WATCH_FEED_like_wlf": {
        "distances": [
          1.860701353864413
        ],
        "weights": [
          0.5146814509109021
        ]
      },
      "s_to_a_WATCH_event_WATCH_FEED_dontLike_wdf": {
        "distances": [
          11.869151698969777
        ],
        "weights": [
          0.6705113438758805
        ]
      },
      "s_to_a_WATCH_event_WATCH_FEED_like_wlf": {
        "distances": [
          -20.037766450197815
        ],
        "weights": [
          0.530257113688939
        ]
      },
      "a_to_s_event_WATCH_FEED_like_wlf_FEED": {
        "distances": [
          -31.348061779238016
        ],
        "weights": [
          0.44183483327358664
        ]
      },
      "rule_from_event_WATCH_FEED_dontLike_wdf_event_wdf_wlf_offLike_offLike": {
        "distances": [
          0.41022929788747514
        ],
        "weights": [
          0.3992442571312311
        ]
      },
      "rule_from_event_WATCH_FEED_like_wlf_event_wlf_fsl_onSee_onSee": {
        "distances": [
          -45.588536740868385
        ],
        "weights": [
          0.36447029400994374
        ]
      },
      "rule_to_event_wdf_fsl_offSee_offSee_event_FEED_LIST_seeList_fsl": {
        "distances": [
          -40.455079088355845
        ],
        "weights": [
          0.44172990050608496
        ]
      },
      "s_to_a_FEED_event_FEED_WATCH_watch_fww": {
        "distances": [
          -48.456751107048824
        ],
        "weights": [
          0.5037094731558273
        ]
      }
    }
  },
  "cyLayout_-605525761": {
    "nodes": {
      "event_x_z_xz_xz": {
        "x": -14.76091878126126,
        "y": 96.515166493838
      },
      "event_z_v_vz_vz": {
        "x": 406.8298934061259,
        "y": 304.29358173722466
      },
      "event_x_x_xx_xx": {
        "x": -21.54396094608859,
        "y": -235.98508631207122
      },
      "event_z_y_zy_zy": {
        "x": 435.899780209225,
        "y": 139.65862182810804
      },
      "event_y_y_yy_yy": {
        "x": 633.7788548249368,
        "y": -182.46737005917078
      },
      "v": {
        "x": 583.4997802092249,
        "y": 319.46047919971113
      },
      "event_y_x_yx_yx": {
        "x": 283.6763050658744,
        "y": -244.10094519336837
      },
      "x": {
        "x": 13.522716725622782,
        "y": -156.50094519336864
      },
      "y": {
        "x": 502.60966040929713,
        "y": -177.58231676223372
      },
      "event_x_y_xy_xy": {
        "x": 293.1691614005053,
        "y": -116.24071879956668
      },
      "event_z_z_zz_zz": {
        "x": 118.65884161888322,
        "y": 323.86230355618005
      },
      "z": {
        "x": 235.21563909052227,
        "y": 264.22460711236
      }
    },
    "edges": {}
  },
  "cyLayout_1453731501": {
    "nodes": {
      "s0": {
        "x": -28.149999999999977,
        "y": 78.75
      },
      "s2": {
        "x": 633.65,
        "y": 78.75
      },
      "s1": {
        "x": 303.05,
        "y": 167.13848167539265
      },
      "event_s1_s2_fechar_fechar": {
        "x": 491.45,
        "y": 78.75
      },
      "event_s0_s1_abrir_abrir": {
        "x": 114.65,
        "y": 62.976910994764395
      },
      "event_abrir_fechar_abrirtaufechar_abrirtaufechar": {
        "x": 305.3032984293194,
        "y": 0.1258115183246158
      }
    },
    "edges": {}
  },
  "cyLayout_name_IntrusiveProduct": {
    "nodes": {
      "w/i0": {
        "x": -55.549999999999955,
        "y": 125.175
      },
      "s/i1": {
        "x": 624.8294651507391,
        "y": -14.499718537714296
      },
      "s/i0": {
        "x": 896.8715839230678,
        "y": 94.28171699086072
      },
      "event_s/i2_s/i0_s/d_s/d": {
        "x": 766.9245649870428,
        "y": 94.28171699086072
      },
      "s/i2": {
        "x": 643.9371355152193,
        "y": 112.28231176257836
      },
      "event_w/a_w/noAs_w/ataunoAs_w/ataunoAs": {
        "x": -27.453450629511316,
        "y": 289.332883530076
      },
      "w/i1": {
        "x": 191.05,
        "y": 192.675
      },
      "event_s/a_s/b_s/ataub_s/ataub": {
        "x": 995.6767144127715,
        "y": 221.62722620106427
      },
      "event_w/a_w/a_w/noAs_w/noAs": {
        "x": 58.39211646992396,
        "y": 288.97085611293875
      },
      "event_w/i1_w/i0_w/c_w/c": {
        "x": 314.05,
        "y": 224.475
      },
      "event_w/c_s/b_w/ctaus/b_w/ctaus/b": {
        "x": 436.45000000000005,
        "y": 224.475
      },
      "event_s/i1_s/i2_s/b_s/b": {
        "x": 618.85,
        "y": 224.475
      },
      "event_s/i0_s/i1_s/a_s/a": {
        "x": 994.176791188107,
        "y": 81.58767911881071
      },
      "event_w/i0_w/i1_w/a_w/a": {
        "x": 37.97260193763819,
        "y": 202.81515192781728
      }
    },
    "edges": {}
  },
  "cyLayout_name_MM": {
    "nodes": {
      "event_x_z_xz_xz": {
        "x": 52.41866681356085,
        "y": 95.80758899457378
      },
      "event_x_x_xx_xx": {
        "x": -5.0151780474513945,
        "y": -232.17004687354287
      },
      "event_z_v_vz_vz": {
        "x": 533.7581052607651,
        "y": 315.10099963158024
      },
      "event_z_z_zz_zz": {
        "x": 345.85445026178024,
        "y": 183.20866492146592
      },
      "event_x_y_xy_xy": {
        "x": 309.6889464257038,
        "y": -206.89715187676632
      },
      "event_xy_zy_xyyz_xyyz": {
        "x": 382.7188944904645,
        "y": 57.83640985799129
      },
      "v": {
        "x": 628.3650834355686,
        "y": 223.8135645506293
      },
      "event_z_y_zy_zy": {
        "x": 592.8341951930663,
        "y": 78.7591595548359
      },
      "z": {
        "x": 357.18345549738217,
        "y": 284.51913612565437
      },
      "x": {
        "x": 21.772756109585167,
        "y": -147.91121320355776
      },
      "y": {
        "x": 593.612570333768,
        "y": -203.5800694403181
      }
    },
    "edges": {}
  },
  "cyLayout_name_LikeAlgorithm": {
    "nodes": {
      "event_like_watchLike_lw_lw": {
        "x": -61.764009754561116,
        "y": 281.530636734963
      },
      "event_Feed_Watch_watch_watch": {
        "x": 274.67749410157086,
        "y": 263.5824833949257
      },
      "event_Feed_List_watchLike_watchLike": {
        "x": 68.71774193548389,
        "y": 482.8104838709677
      },
      "Watch": {
        "x": 632.774193548387,
        "y": 274.61532258064517
      },
      "event_Watch_Watch_like_like": {
        "x": 667.4854838709678,
        "y": 200.5496009873571
      },
      "event_List_Watch_watch2_watch2": {
        "x": 504.01033328729034,
        "y": 429.0060284114916
      },
      "event_like_refresh_lr_lr": {
        "x": 530.7161627375671,
        "y": 147.92726143504703
      },
      "event_Watch_Feed_refresh_refresh": {
        "x": 271.87782679380865,
        "y": 191.2442053452452
      },
      "event_like_dontLike_ld_ld": {
        "x": 433.2990782410911,
        "y": 291.93359849819467
      },
      "Feed": {
        "x": 31.858064516129147,
        "y": 337.22983870967744
      },
      "event_Watch_Feed_dontLike_dontLike": {
        "x": 293.09332414690033,
        "y": 409.48796845599855
      },
      "event_dontLike_watchLike_dw_dw": {
        "x": 192.62040347338683,
        "y": 463.33688829643836
      },
      "event_watch_dontLike_wd_wd": {
        "x": 281.3267223893484,
        "y": 326.5196081751485
      },
      "List": {
        "x": 355.04652358415706,
        "y": 506.23284027593235
      }
    },
    "edges": {
      "s_to_a_Feed_event_Feed_Watch_watch_watch": {
        "distances": [
          -13.233004178971447
        ],
        "weights": [
          0.45854064976010045
        ]
      },
      "rule_from_event_Feed_Watch_watch_watch_event_watch_dontLike_wd_wd": {
        "distances": [
          4.090288241509921
        ],
        "weights": [
          0.5647172984863159
        ]
      },
      "rule_to_event_dontLike_watchLike_dw_dw_event_Feed_List_watchLike_watchLike": {
        "distances": [
          -8.36207174741842
        ],
        "weights": [
          0.3876542742152796
        ]
      },
      "a_to_s_event_Watch_Feed_dontLike_dontLike_Feed": {
        "distances": [
          -17.3683827274835
        ],
        "weights": [
          0.6370475032541367
        ]
      },
      "rule_from_event_Watch_Watch_like_like_event_like_watchLike_lw_lw": {
        "distances": [
          385.2927474269914
        ],
        "weights": [
          0.16488623686234377
        ]
      },
      "rule_to_event_watch_dontLike_wd_wd_event_Watch_Feed_dontLike_dontLike": {
        "distances": [
          -3.100247225991664
        ],
        "weights": [
          0.46837672646206274
        ]
      },
      "rule_from_event_Watch_Feed_dontLike_dontLike_event_dontLike_watchLike_dw_dw": {
        "distances": [
          -36.17041353674074
        ],
        "weights": [
          0.25877532592682195
        ]
      },
      "a_to_s_event_Feed_Watch_watch_watch_Watch": {
        "distances": [
          -12.435335908596395
        ],
        "weights": [
          0.3958850173542578
        ]
      }
    }
  },
  "cyLayout_name_TIMER": {
    "nodes": {
      "s0": {
        "x": -14.049999999999983,
        "y": 57
      },
      "event_s1_s0_escape_escape": {
        "x": 120.54475806451616,
        "y": -19.45766129032258
      },
      "event_s1_s2_timeout_timeout": {
        "x": 355.55,
        "y": 106.19999999999999
      },
      "s2": {
        "x": 478.54999999999995,
        "y": 106.19999999999999
      },
      "s1": {
        "x": 232.55,
        "y": 94.19999999999999
      },
      "event_s0_s1_start_start": {
        "x": 102.18306451612905,
        "y": 177.4463709677419
      }
    },
    "edges": {}
  },
  "cyLayout_name_DroneSystem": {
    "nodes": {
      "event_Flying_Crashed_fail_fail": {
        "x": 635.9329401475308,
        "y": -4.790720613346147
      },
      "event_fail_return_failtaureturn_failtaureturn": {
        "x": 481.37270527969275,
        "y": 260.5036030667307
      },
      "Flying": {
        "x": 537.1657206133461,
        "y": -109.0813768321845
      },
      "event_Home_Flying_launch_launch": {
        "x": 330.04531266306924,
        "y": -101.98070721262297
      },
      "Delivered": {
        "x": 355.67378694078434,
        "y": 99.77181158390775
      },
      "Crashed": {
        "x": 846.55,
        "y": 10.424999999999997
      },
      "event_Flying_Delivered_success_success": {
        "x": 464.5024879038308,
        "y": 3.3554213510000728
      },
      "event_Delivered_Home_return_return": {
        "x": 204.71756979031537,
        "y": 148.79422839286923
      },
      "Home": {
        "x": 162.90015968171545,
        "y": -1.5569511645384004
      }
    },
    "edges": {}
  },
  "cyLayout_name_Vending1eur": {
    "nodes": {
      "Coffee": {
        "x": 105.91868279569894,
        "y": 43.49811827956988
      },
      "event_Insert_Coffee_ct50_ct50": {
        "x": 284.97150537634406,
        "y": 114.70981182795698
      },
      "event_Insert_Chocolate_eur1_eur1": {
        "x": 488.54368279569906,
        "y": 176.59422043010755
      },
      "event_ct50_lastct50_ct50taulastct50_ct50taulastct50": {
        "x": 165.0018817204301,
        "y": 132.21747311827954
      },
      "event_Chocolate_Insert_GetChoc_GetChoc": {
        "x": 648.3400537634408,
        "y": 225.6331989247312
      },
      "event_ct50_ct50_lastct50_lastct50": {
        "x": 238.67809139784944,
        "y": 208.4094086021505
      },
      "Insert": {
        "x": 397.49448924731183,
        "y": 351.62298387096774
      },
      "Chocolate": {
        "x": 647.7741935483871,
        "y": 96.9502688172043
      },
      "event_ct50_eur1_ct50taueur1_ct50taueur1": {
        "x": 391.172062356186,
        "y": 133.6202565033426
      },
      "event_Coffee_Insert_GetCoffee_GetCoffee": {
        "x": 64.00739247311829,
        "y": 250.42741935483872
      },
      "event_eur1_eur1_eur1taueur1_eur1taueur1": {
        "x": 547.8279569892472,
        "y": 244.1807795698925
      },
      "event_eur1_ct50_eur1tauct50_eur1tauct50": {
        "x": 396.3450465448267,
        "y": 37.00782478238991
      }
    },
    "edges": {
      "s_to_a_Chocolate_event_Chocolate_Insert_GetChoc_GetChoc": {
        "distances": [
          -51.27159652465808
        ],
        "weights": [
          0.5085392963258174
        ]
      },
      "rule_to_event_ct50_eur1_ct50taueur1_ct50taueur1_event_Insert_Chocolate_eur1_eur1": {
        "distances": [
          -4.368942729504252
        ],
        "weights": [
          0.37323615428847745
        ]
      },
      "a_to_s_event_Insert_Chocolate_eur1_eur1_Chocolate": {
        "distances": [
          -34.125806153583696
        ],
        "weights": [
          0.4936241486843837
        ]
      },
      "s_to_a_Insert_event_Insert_Chocolate_eur1_eur1": {
        "distances": [
          -27.153155173060494
        ],
        "weights": [
          0.641302562407621
        ]
      },
      "a_to_s_event_Chocolate_Insert_GetChoc_GetChoc_Insert": {
        "distances": [
          -113.11203993682541
        ],
        "weights": [
          0.42603402728971124
        ]
      },
      "rule_from_event_Insert_Coffee_ct50_ct50_event_ct50_eur1_ct50taueur1_ct50taueur1": {
        "distances": [
          -3.6197219168344366
        ],
        "weights": [
          0.43288292325096633
        ]
      }
    }
  },
  "cyLayout_name_ROAD": {
    "nodes": {
      "P9a2": {
        "x": 1901.7977859755144,
        "y": -601.752524876894
      },
      "P31": {
        "x": 2764.35,
        "y": 79.45000000000002
      },
      "event_P4_P5_t_p4_p5_t_p4_p5": {
        "x": 3133.35,
        "y": 203.05
      },
      "P8a": {
        "x": 2734.77800216683,
        "y": -706.0331021407061
      },
      "P11": {
        "x": 1598.9614184779944,
        "y": -552.676143755379
      },
      "P4": {
        "x": 3010.35,
        "y": 203.05
      },
      "event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1": {
        "x": 4117.35,
        "y": 350.95
      },
      "event_P3_P31_t_p3_p31_t_p3_p31": {
        "x": 2641.35,
        "y": 79.45000000000002
      },
      "P9a1": {
        "x": 2761.70543722492,
        "y": -506.49609547578103
      },
      "P22": {
        "x": 3748.35,
        "y": -19.25
      },
      "event_P22_P7_t_p22_p7_t_p22_p7": {
        "x": 3871.35,
        "y": -19.25
      },
      "event_P9a2_P11_t_p9a2_p11_t_p9a2_p11": {
        "x": 1742.744924006112,
        "y": -590.8964408431348
      },
      "event_P14_P15_t_p14_p15_t_p14_p15": {
        "x": 932.8201243124065,
        "y": -273.3235682361233
      },
      "event_P16_A_t_p16_a_t_p16_a": {
        "x": 296.9873864540613,
        "y": -287.22597486611835
      },
      "event_P10_P11_t_p10_p11_t_p10_p11": {
        "x": 1805.1987623939624,
        "y": -544.7194267680419
      },
      "P7a2": {
        "x": 4240.35,
        "y": 474.54999999999995
      },
      "event_P3_P32_t_p3_p32_t_p3_p32": {
        "x": 2641.35,
        "y": 203.05
      },
      "P61": {
        "x": 3748.35,
        "y": 141.25
      },
      "event_D_P1_t_d_p1_t_d_p1": {
        "x": 1657.35,
        "y": 79.45000000000002
      },
      "P9": {
        "x": 4486.35,
        "y": 79.45000000000002
      },
      "event_P31_P4a_t_p31_p4a_t_p31_p4a": {
        "x": 2887.35,
        "y": 79.45000000000002
      },
      "event_P6_P61_t_p6_p61_t_p6_p61": {
        "x": 3625.35,
        "y": 141.25
      },
      "event_P2_P22_t_p2_p22_t_p2_p22": {
        "x": 3625.35,
        "y": -19.25
      },
      "event_P13_P14_t_p13_p14_t_p13_p14": {
        "x": 1012.0206337579915,
        "y": -499.27541956140936
      },
      "event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2": {
        "x": 4117.35,
        "y": 474.54999999999995
      },
      "P9a": {
        "x": 2461.346306257683,
        "y": -650.2730055815102
      },
      "event_P8_P9_t_p8_p9_t_p8_p9": {
        "x": 4363.35,
        "y": 79.45000000000002
      },
      "event_P62_P7a_t_p62_p7a_t_p62_p7a": {
        "x": 3871.35,
        "y": 412.75
      },
      "P6": {
        "x": 3502.35,
        "y": 141.25
      },
      "event_P1_P2_t_p1_p2_t_p1_p2": {
        "x": 1903.35,
        "y": 79.45000000000002
      },
      "event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1": {
        "x": 2554.146789053663,
        "y": -536.6956126798013
      },
      "P5": {
        "x": 3256.35,
        "y": 141.25
      },
      "event_P4a_P5_t_p4a_p5_t_p4a_p5": {
        "x": 3133.35,
        "y": 79.45000000000002
      },
      "P4a": {
        "x": 3010.35,
        "y": 79.45000000000002
      },
      "event_P15_P16_t_p15_p16_t_p15_p16": {
        "x": 622.7238591126061,
        "y": -275.0613690648728
      },
      "P10": {
        "x": 4732.35,
        "y": 79.45000000000002
      },
      "event_P9_P10_t_p9_p10_t_p9_p10": {
        "x": 4609.35,
        "y": 79.45000000000002
      },
      "P62": {
        "x": 3748.35,
        "y": 412.75
      },
      "P13": {
        "x": 1112.4970610677408,
        "y": -511.3552264430174
      },
      "P1": {
        "x": 1780.35,
        "y": 79.45000000000002
      },
      "event_P2_P21_t_p2_p21_t_p2_p21": {
        "x": 2132.0047227510486,
        "y": 170.15879541491856
      },
      "event_P9a1_P14_t_p9a1_p14_t_p9a1_p14": {
        "x": 1552.9067285276296,
        "y": -398.55595063698706
      },
      "event_P32_P4_t_p32_p4_t_p32_p4": {
        "x": 2887.35,
        "y": 203.05
      },
      "event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2": {
        "x": 2243.0917618522553,
        "y": -657.0910261179929
      },
      "event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a": {
        "x": 4363.35,
        "y": 350.95
      },
      "P7a1": {
        "x": 4240.35,
        "y": 350.95
      },
      "event_P12_P13_t_p12_p13_t_p12_p13": {
        "x": 1249.2129090223145,
        "y": -526.4549850450275
      },
      "event_P61_P7_t_p61_p7_t_p61_p7": {
        "x": 3871.35,
        "y": 141.25
      },
      "event_P5_P6_t_p5_p6_t_p5_p6": {
        "x": 3379.35,
        "y": 141.25
      },
      "D": {
        "x": 30.552283131789558,
        "y": 11.675767678774342
      },
      "P21": {
        "x": 2272.35,
        "y": 141.25
      },
      "event_P21_P3_t_p21_p3_t_p21_p3": {
        "x": 2395.35,
        "y": 141.25
      },
      "event_P11_P12_t_p11_p12_t_p11_p12": {
        "x": 1480.3652808458335,
        "y": -541.5547436470375
      },
      "event_P6_P62_t_p6_p62_t_p6_p62": {
        "x": 3625.35,
        "y": 412.75
      },
      "P8": {
        "x": 4240.35,
        "y": 79.45000000000002
      },
      "event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa": {
        "x": 4363.35,
        "y": 474.54999999999995
      },
      "event_A_D_t_a_d_t_a_d": {
        "x": 21.64713782924852,
        "y": -136.03730276492263
      },
      "P16": {
        "x": 478.10253148520223,
        "y": -285.488174037369
      },
      "P12": {
        "x": 1364.7890949340738,
        "y": -526.4549850450275
      },
      "P3": {
        "x": 2518.35,
        "y": 141.25
      },
      "P15": {
        "x": 793.4121991712508,
        "y": -266.37236492112584
      },
      "P2": {
        "x": 2026.35,
        "y": 79.45000000000002
      },
      "P7": {
        "x": 3994.35,
        "y": 79.45000000000002
      },
      "event_P8a_P9a_t_p8a_p9a_t_p8a_p9a": {
        "x": 2592.0222507714525,
        "y": -693.953295259098
      },
      "A": {
        "x": 121.08564390916857,
        "y": -262.89676326362707
      },
      "P8aa": {
        "x": 4486.35,
        "y": 474.54999999999995
      },
      "event_P7_P8_t_p7_p8_t_p7_p8": {
        "x": 4117.35,
        "y": 79.45000000000002
      },
      "P7a": {
        "x": 3994.35,
        "y": 412.75
      },
      "event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a": {
        "x": 4609.35,
        "y": 474.54999999999995
      },
      "P14": {
        "x": 944.7636753726644,
        "y": -399.61701278814274
      },
      "P32": {
        "x": 2764.35,
        "y": 203.05
      }
    },
    "edges": {}
  },
  "cyLayout_name_Lines": {
    "nodes": {
      "event_P10_P11_t_p10_p11_t_p10_p11": {
        "x": 2205.7368018798848,
        "y": -198.242233984367
      },
      "P9a2": {
        "x": 2435.3332059529703,
        "y": -280.0280795613601
      },
      "P61": {
        "x": 2997.179730583091,
        "y": -593.9657523080659
      },
      "P31": {
        "x": 2680.7543993733716,
        "y": -760.9685537963785
      },
      "event_P4_P5_t_p4_p5_t_p4_p5": {
        "x": 3261.800076428218,
        "y": -882.9157613159198
      },
      "P8a": {
        "x": 2767.4309251924733,
        "y": -513.7205011632469
      },
      "P11": {
        "x": 2206.0312736773562,
        "y": -60.042629475436044
      },
      "P4": {
        "x": 3091,
        "y": -880
      },
      "event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1": {
        "x": 3090.7648814412455,
        "y": -442.27368018798853
      },
      "event_P3_P31_t_p3_p31_t_p3_p31": {
        "x": 2533.8315226318396,
        "y": -754.7719968668579
      },
      "P9a1": {
        "x": 2791.877199686686,
        "y": -188.38183979290636
      },
      "P22": {
        "x": 2392.4035959269154,
        "y": -660.4350421305368
      },
      "event_P22_P7_t_p22_p7_t_p22_p7": {
        "x": 2687.5039052021557,
        "y": -657.0820138398407
      },
      "event_D_P1_df_df": {
        "x": 1757,
        "y": -601.9999999999999
      },
      "event_P9a2_P11_t_p9a2_p11_t_p9a2_p11": {
        "x": 2320.8034430704793,
        "y": -173.09808739421547
      },
      "event_P14_P15_t_p14_p15_t_p14_p15": {
        "x": 2690.64038139011,
        "y": 161.87502116198004
      },
      "P7a2": {
        "x": 3093.3788830078165,
        "y": -238.94384087727977
      },
      "event_P3_P32_t_p3_p32_t_p3_p32": {
        "x": 2649.452957476541,
        "y": -869.3836619418379
      },
      "P9": {
        "x": 2208.4311558991562,
        "y": -480.98582783386496
      },
      "event_P31_P4a_t_p31_p4a_t_p31_p4a": {
        "x": 2842.0175974934864,
        "y": -762.7895943603442
      },
      "event_P6_P61_t_p6_p61_t_p6_p61": {
        "x": 3141.128219446064,
        "y": -593.8375328620018
      },
      "event_P2_P22_t_p2_p22_t_p2_p22": {
        "x": 2045.052410339369,
        "y": -657.2280031331421
      },
      "event_P13_P14_t_p13_p14_t_p13_p14": {
        "x": 2826.894146183737,
        "y": 37.86310599640379
      },
      "event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2": {
        "x": 3092,
        "y": -310
      },
      "P9a": {
        "x": 2771.0766589511354,
        "y": -351.40512032178685
      },
      "event_P8_P9_t_p8_p9_t_p8_p9": {
        "x": 2212.060282458698,
        "y": -584.9999999999999
      },
      "event_P62_P7a_t_p62_p7a_t_p62_p7a": {
        "x": 3247.3294256126537,
        "y": -368.44528690003074
      },
      "P6": {
        "x": 3249.8491201253255,
        "y": -593.1228003133142
      },
      "event_P1_P2_t_p1_p2_t_p1_p2": {
        "x": 1916,
        "y": -798
      },
      "event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1": {
        "x": 2781.3721497232937,
        "y": -277.45263962402294
      },
      "P5": {
        "x": 3260.168477368161,
        "y": -773.5578424438509
      },
      "event_P4a_P5_t_p4a_p5_t_p4a_p5": {
        "x": 3133.480719185383,
        "y": -773.1860748616468
      },
      "P4a": {
        "x": 3019.5544758015894,
        "y": -762.4878346109956
      },
      "event_P15_P16_t_p15_p16_t_p15_p16": {
        "x": 2173,
        "y": 68.00000000000003
      },
      "P10": {
        "x": 2206.3579188720696,
        "y": -293.06664119059394
      },
      "event_P9_P10_t_p9_p10_t_p9_p10": {
        "x": 2208.210405639656,
        "y": -378.0595257649816
      },
      "P62": {
        "x": 3248.7263198120118,
        "y": -443.2948207234116
      },
      "P13": {
        "x": 2774.0821092951005,
        "y": -54.955642516783925
      },
      "P1": {
        "x": 1767.9999999999998,
        "y": -798
      },
      "event_P2_P21_t_p2_p21_t_p2_p21": {
        "x": 2068,
        "y": -875
      },
      "event_P16_D_to_F_to_F": {
        "x": 1857.9978912143945,
        "y": -276.078813844689
      },
      "event_P9a1_P14_t_p9a1_p14_t_p9a1_p14": {
        "x": 2940.1244806063905,
        "y": -51.0160897592024
      },
      "event_P32_P4_t_p32_p4_t_p32_p4": {
        "x": 2935,
        "y": -870
      },
      "event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2": {
        "x": 2530.444079364901,
        "y": -360.41314739239255
      },
      "event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a": {
        "x": 2931.0851773250924,
        "y": -517.7110370160144
      },
      "P7a1": {
        "x": 3095.3403213785828,
        "y": -526.1123182454403
      },
      "event_P12_P13_t_p12_p13_t_p12_p13": {
        "x": 2633.9056613891357,
        "y": -53.228003133142145
      },
      "event_P61_P7_t_p61_p7_t_p61_p7": {
        "x": 2851.1712384596703,
        "y": -587.2221912001943
      },
      "event_P5_P6_t_p5_p6_t_p5_p6": {
        "x": 3257,
        "y": -681
      },
      "D": {
        "x": 1796.5618702293984,
        "y": -438.78796183177457
      },
      "P21": {
        "x": 2249,
        "y": -877
      },
      "event_P21_P3_t_p21_p3_t_p21_p3": {
        "x": 2391,
        "y": -874
      },
      "event_P11_P12_t_p11_p12_t_p11_p12": {
        "x": 2352.3467305904364,
        "y": -52.71520527308084
      },
      "event_P6_P62_t_p6_p62_t_p6_p62": {
        "x": 3247.9076497762894,
        "y": -530.485395502281
      },
      "P8": {
        "x": 2368.8208278134116,
        "y": -586.2564388921284
      },
      "event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa": {
        "x": 3099.672944915589,
        "y": -157.90710275817446
      },
      "P16": {
        "x": 2019,
        "y": -34.000000000000036
      },
      "P12": {
        "x": 2488.0234319904307,
        "y": -51.12811513809256
      },
      "P3": {
        "x": 2526,
        "y": -871
      },
      "P15": {
        "x": 2365.882680114065,
        "y": 165.20462923041907
      },
      "P2": {
        "x": 2044,
        "y": -791
      },
      "P7": {
        "x": 2688.7695958746363,
        "y": -589.7863009232263
      },
      "event_P8a_P9a_t_p8a_p9a_t_p8a_p9a": {
        "x": 2767.8118299061634,
        "y": -435.3143664071349
      },
      "P8aa": {
        "x": 2924.2511063950556,
        "y": -160.25553197527785
      },
      "event_P7_P8_t_p7_p8_t_p7_p8": {
        "x": 2514.590144489797,
        "y": -581.0854796307095
      },
      "P7a": {
        "x": 3092.6982402506515,
        "y": -373.27368018798853
      },
      "event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a": {
        "x": 2917.918730180581,
        "y": -328.21773470649714
      },
      "P14": {
        "x": 2968.6320774977007,
        "y": 143.2027017419702
      },
      "P32": {
        "x": 2798,
        "y": -871
      }
    },
    "edges": {
      "s_to_a_P7a2_event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa": {
        "distances": [
          1.0945374833792103
        ],
        "weights": [
          0.5254153411719398
        ]
      },
      "a_to_s_event_P14_P15_t_p14_p15_t_p14_p15_P15": {
        "distances": [
          -10.096395904085002
        ],
        "weights": [
          0.6142733840810428
        ]
      },
      "a_to_s_event_P8a_P9a_t_p8a_p9a_t_p8a_p9a_P9a": {
        "distances": [
          6.214525527178343
        ],
        "weights": [
          0.456807342815261
        ]
      },
      "a_to_s_event_P9_P10_t_p9_p10_t_p9_p10_P10": {
        "distances": [
          -1.189110741509285
        ],
        "weights": [
          0.45385861343767403
        ]
      },
      "a_to_s_event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a_P8a": {
        "distances": [
          4.774095778940595
        ],
        "weights": [
          0.5129792627625116
        ]
      },
      "a_to_s_event_P2_P22_t_p2_p22_t_p2_p22_P22": {
        "distances": [
          8.071867193548387
        ],
        "weights": [
          0.47888102745660077
        ]
      },
      "s_to_a_P32_event_P32_P4_t_p32_p4_t_p32_p4": {
        "distances": [
          8.197944494114392
        ],
        "weights": [
          0.4635467123404698
        ]
      },
      "s_to_a_P2_event_P2_P21_t_p2_p21_t_p2_p21": {
        "distances": [
          -3.202313426437055
        ],
        "weights": [
          0.34133922244306747
        ]
      },
      "a_to_s_event_P6_P62_t_p6_p62_t_p6_p62_P62": {
        "distances": [
          7.217662948313478
        ],
        "weights": [
          0.41605982482431136
        ]
      },
      "a_to_s_event_P4a_P5_t_p4a_p5_t_p4a_p5_P5": {
        "distances": [
          13.42029028601789
        ],
        "weights": [
          0.5039089166928091
        ]
      },
      "s_to_a_P4_event_P4_P5_t_p4_p5_t_p4_p5": {
        "distances": [
          10.041249358516463
        ],
        "weights": [
          0.49873741842237856
        ]
      },
      "a_to_s_event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a_P9a": {
        "distances": [
          1.2953067079886995
        ],
        "weights": [
          0.5551622487911034
        ]
      },
      "s_to_a_P9a2_event_P9a2_P11_t_p9a2_p11_t_p9a2_p11": {
        "distances": [
          -1.2700568450644283
        ],
        "weights": [
          0.5064345219768094
        ]
      },
      "a_to_s_event_P22_P7_t_p22_p7_t_p22_p7_P7": {
        "distances": [
          -0.07267180530930178
        ],
        "weights": [
          0.3360943077952167
        ]
      },
      "s_to_a_D_event_D_P1_df_df": {
        "distances": [
          -2.7649370689977215
        ],
        "weights": [
          0.5278944021584638
        ]
      },
      "s_to_a_P7a_event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1": {
        "distances": [
          0.4273577127872325
        ],
        "weights": [
          0.38523319263796557
        ]
      },
      "a_to_s_event_P9a1_P14_t_p9a1_p14_t_p9a1_p14_P14": {
        "distances": [
          -1.1595086616994803
        ],
        "weights": [
          0.42522912541829955
        ]
      },
      "a_to_s_event_P10_P11_t_p10_p11_t_p10_p11_P11": {
        "distances": [
          -7.548722438918125
        ],
        "weights": [
          0.49138851410586737
        ]
      },
      "s_to_a_P9a_event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1": {
        "distances": [
          -0.2705180435622271
        ],
        "weights": [
          0.5088659786604878
        ]
      },
      "s_to_a_P5_event_P5_P6_t_p5_p6_t_p5_p6": {
        "distances": [
          2.3310525328352476
        ],
        "weights": [
          0.5798796804467459
        ]
      },
      "s_to_a_P13_event_P13_P14_t_p13_p14_t_p13_p14": {
        "distances": [
          1.7007298023608266
        ],
        "weights": [
          0.47612980981898606
        ]
      },
      "a_to_s_event_P32_P4_t_p32_p4_t_p32_p4_P4": {
        "distances": [
          10.036349414637755
        ],
        "weights": [
          0.3653831562060871
        ]
      },
      "s_to_a_P6_event_P6_P62_t_p6_p62_t_p6_p62": {
        "distances": [
          2.6324392988209344
        ],
        "weights": [
          0.5142256966983064
        ]
      },
      "s_to_a_P62_event_P62_P7a_t_p62_p7a_t_p62_p7a": {
        "distances": [
          7.794337209807774
        ],
        "weights": [
          0.5181061362397678
        ]
      },
      "a_to_s_event_P15_P16_t_p15_p16_t_p15_p16_P16": {
        "distances": [
          -7.644861695207878
        ],
        "weights": [
          0.4663977409322675
        ]
      },
      "a_to_s_event_P8_P9_t_p8_p9_t_p8_p9_P9": {
        "distances": [
          2.681888683388086
        ],
        "weights": [
          0.5789649059657765
        ]
      },
      "a_to_s_event_P9a2_P11_t_p9a2_p11_t_p9a2_p11_P11": {
        "distances": [
          -0.15079933015025793
        ],
        "weights": [
          0.4809647671985674
        ]
      },
      "s_to_a_P9a_event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2": {
        "distances": [
          0.18708620893415606
        ],
        "weights": [
          0.5261624444038542
        ]
      },
      "s_to_a_P4a_event_P4a_P5_t_p4a_p5_t_p4a_p5": {
        "distances": [
          4.180528705457987
        ],
        "weights": [
          0.47672938147250843
        ]
      },
      "a_to_s_event_P2_P21_t_p2_p21_t_p2_p21_P21": {
        "distances": [
          5.213098956432523
        ],
        "weights": [
          0.2939901220712554
        ]
      },
      "a_to_s_event_P13_P14_t_p13_p14_t_p13_p14_P14": {
        "distances": [
          0.5639937390058848
        ],
        "weights": [
          0.47861167732432935
        ]
      },
      "a_to_s_event_P62_P7a_t_p62_p7a_t_p62_p7a_P7a": {
        "distances": [
          -1.162986735578697
        ],
        "weights": [
          0.38158844590416585
        ]
      },
      "s_to_a_P9a1_event_P9a1_P14_t_p9a1_p14_t_p9a1_p14": {
        "distances": [
          -1.3717995477892382
        ],
        "weights": [
          0.4461242559820761
        ]
      },
      "s_to_a_P8a_event_P8a_P9a_t_p8a_p9a_t_p8a_p9a": {
        "distances": [
          -4.583418887191594
        ],
        "weights": [
          0.5644709409355415
        ]
      },
      "s_to_a_P14_event_P14_P15_t_p14_p15_t_p14_p15": {
        "distances": [
          5.490093033469452
        ],
        "weights": [
          0.5890304805022102
        ]
      },
      "s_to_a_P8aa_event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a": {
        "distances": [
          -15.310457300014242
        ],
        "weights": [
          0.550418248101698
        ]
      },
      "a_to_s_event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1_P7a1": {
        "distances": [
          -4.39745335044915
        ],
        "weights": [
          0.4808210086982788
        ]
      },
      "a_to_s_event_P7_P8_t_p7_p8_t_p7_p8_P8": {
        "distances": [
          2.3619150082195794
        ],
        "weights": [
          0.4926439822406519
        ]
      },
      "s_to_a_P9_event_P9_P10_t_p9_p10_t_p9_p10": {
        "distances": [
          4.791697908924452
        ],
        "weights": [
          0.5293185229853281
        ]
      },
      "a_to_s_event_P12_P13_t_p12_p13_t_p12_p13_P13": {
        "distances": [
          -1.8356376565485644
        ],
        "weights": [
          0.5387479207309417
        ]
      },
      "a_to_s_event_P4_P5_t_p4_p5_t_p4_p5_P5": {
        "distances": [
          1.1522784470345264
        ],
        "weights": [
          0.4412199575566384
        ]
      },
      "s_to_a_P16_event_P16_D_to_F_to_F": {
        "distances": [
          -0.17812358731376746
        ],
        "weights": [
          0.5096688942661314
        ]
      },
      "a_to_s_event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2_P7a2": {
        "distances": [
          2.0012396417275036
        ],
        "weights": [
          0.4954630973515798
        ]
      },
      "s_to_a_P3_event_P3_P31_t_p3_p31_t_p3_p31": {
        "distances": [
          -2.4162991107212024
        ],
        "weights": [
          0.5058034434327692
        ]
      },
      "a_to_s_event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1_P9a1": {
        "distances": [
          2.1412225400799336
        ],
        "weights": [
          0.4415725308377675
        ]
      },
      "s_to_a_P11_event_P11_P12_t_p11_p12_t_p11_p12": {
        "distances": [
          4.3908431497079725
        ],
        "weights": [
          0.5267381092353445
        ]
      },
      "s_to_a_P7_event_P7_P8_t_p7_p8_t_p7_p8": {
        "distances": [
          -1.0211071844502946
        ],
        "weights": [
          0.6626217687755693
        ]
      },
      "a_to_s_event_P6_P61_t_p6_p61_t_p6_p61_P61": {
        "distances": [
          -0.25648077889113036
        ],
        "weights": [
          0.5276527534306457
        ]
      },
      "a_to_s_event_P16_D_to_F_to_F_D": {
        "distances": [
          -10.113792366020006
        ],
        "weights": [
          0.5232041111437413
        ]
      },
      "s_to_a_P15_event_P15_P16_t_p15_p16_t_p15_p16": {
        "distances": [
          9.697305674959566
        ],
        "weights": [
          0.4937415734057811
        ]
      },
      "a_to_s_event_P1_P2_t_p1_p2_t_p1_p2_P2": {
        "distances": [
          2.330573824615695
        ],
        "weights": [
          0.4276050235232787
        ]
      },
      "s_to_a_P3_event_P3_P32_t_p3_p32_t_p3_p32": {
        "distances": [
          4.767444352714022
        ],
        "weights": [
          0.23461060023521832
        ]
      },
      "s_to_a_P61_event_P61_P7_t_p61_p7_t_p61_p7": {
        "distances": [
          -7.028577438119633
        ],
        "weights": [
          0.5586561034084216
        ]
      },
      "s_to_a_P7a1_event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a": {
        "distances": [
          1.3156589442962614
        ],
        "weights": [
          0.5974134704872403
        ]
      },
      "a_to_s_event_P3_P31_t_p3_p31_t_p3_p31_P31": {
        "distances": [
          9.290530738309215
        ],
        "weights": [
          0.46592246288821
        ]
      },
      "s_to_a_P22_event_P22_P7_t_p22_p7_t_p22_p7": {
        "distances": [
          7.3609749950245735
        ],
        "weights": [
          0.44531876858220754
        ]
      },
      "s_to_a_P10_event_P10_P11_t_p10_p11_t_p10_p11": {
        "distances": [
          6.214008719827495
        ],
        "weights": [
          0.5670036164999658
        ]
      },
      "a_to_s_event_P3_P32_t_p3_p32_t_p3_p32_P32": {
        "distances": [
          3.747914723197855
        ],
        "weights": [
          0.34828419137591976
        ]
      },
      "a_to_s_event_P61_P7_t_p61_p7_t_p61_p7_P7": {
        "distances": [
          -2.0391528288318215
        ],
        "weights": [
          0.525333206429449
        ]
      },
      "s_to_a_P12_event_P12_P13_t_p12_p13_t_p12_p13": {
        "distances": [
          -3.863149532465997
        ],
        "weights": [
          0.48644532715374716
        ]
      },
      "a_to_s_event_P11_P12_t_p11_p12_t_p11_p12_P12": {
        "distances": [
          2.5291460629439557
        ],
        "weights": [
          0.4220288957451661
        ]
      },
      "a_to_s_event_P5_P6_t_p5_p6_t_p5_p6_P6": {
        "distances": [
          4.276088516717775
        ],
        "weights": [
          0.41551460405800417
        ]
      },
      "a_to_s_event_P31_P4a_t_p31_p4a_t_p31_p4a_P4a": {
        "distances": [
          6.019454882109193
        ],
        "weights": [
          0.44023577303144223
        ]
      },
      "s_to_a_P7a_event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2": {
        "distances": [
          -3.44560638646909
        ],
        "weights": [
          0.5715695580888575
        ]
      },
      "s_to_a_P2_event_P2_P22_t_p2_p22_t_p2_p22": {
        "distances": [
          -0.7016967183524132
        ],
        "weights": [
          0.5573087603521514
        ]
      },
      "a_to_s_event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2_P9a2": {
        "distances": [
          9.169074820280535
        ],
        "weights": [
          0.45086813575244483
        ]
      },
      "a_to_s_event_D_P1_df_df_P1": {
        "distances": [
          -2.4492504404541644
        ],
        "weights": [
          0.4891189477211498
        ]
      },
      "s_to_a_P31_event_P31_P4a_t_p31_p4a_t_p31_p4a": {
        "distances": [
          0.7133436324947633
        ],
        "weights": [
          0.4822373295479973
        ]
      },
      "a_to_s_event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa_P8aa": {
        "distances": [
          0.13036664069581394
        ],
        "weights": [
          0.500410050725852
        ]
      },
      "a_to_s_event_P21_P3_t_p21_p3_t_p21_p3_P3": {
        "distances": [
          15.38664074555459
        ],
        "weights": [
          0.5290372597102201
        ]
      },
      "s_to_a_P21_event_P21_P3_t_p21_p3_t_p21_p3": {
        "distances": [
          4.31366582430282
        ],
        "weights": [
          0.46846570311932184
        ]
      },
      "s_to_a_P1_event_P1_P2_t_p1_p2_t_p1_p2": {
        "distances": [
          1.8148404659046362
        ],
        "weights": [
          0.5370247310463049
        ]
      },
      "s_to_a_P6_event_P6_P61_t_p6_p61_t_p6_p61": {
        "distances": [
          -4.1398152897448774
        ],
        "weights": [
          0.5081207723618868
        ]
      },
      "s_to_a_P8_event_P8_P9_t_p8_p9_t_p8_p9": {
        "distances": [
          -9.038325041862995
        ],
        "weights": [
          0.5432268710586388
        ]
      }
    }
  },
  "cyLayout_1388087358": {
    "nodes": {
      "event_z_z_zz_zz": {
        "x": 347.8001047120419,
        "y": 245.41298429319372
      },
      "event_z_y_zy_zy": {
        "x": 523.6409424083771,
        "y": 62.86534031413613
      },
      "event_x_x_xx_xx": {
        "x": 162.23361256544501,
        "y": -222.97089005235597
      },
      "z": {
        "x": 353.8742408376963,
        "y": 128.51287958115182
      },
      "x": {
        "x": 203.01036649214657,
        "y": -134.30408376963345
      },
      "y": {
        "x": 554.4394764397905,
        "y": -105.09005235602089
      },
      "event_x_z_xz_xz": {
        "x": 219.9152879581152,
        "y": 13.18973821989529
      }
    },
    "edges": {}
  },
  "cyLayout_name_Counter": {
    "nodes": {
      "s0": {
        "x": -13.899999999999977,
        "y": 69.55
      },
      "event_act_offAct_on1_on1": {
        "x": 200.7557795698925,
        "y": 115.01465053763434
      },
      "event_s0_s0_act_act": {
        "x": 109.7,
        "y": 69.55
      },
      "event_act_on1_acttauon1_acttauon1": {
        "x": 135.73400537634404,
        "y": 158.8557795698925
      },
      "event_act_act_offAct_offAct": {
        "x": 186.33077956989246,
        "y": 19.967607526881743
      }
    },
    "edges": {}
  },
  "cyLayout_name_Vending": {
    "nodes": {
      "event_ct50_lastct50_ct50taulastct50_ct50taulastct50": {
        "x": -45.44999999999993,
        "y": 178.35
      },
      "event_ct50_ct50_lastct50_lastct50": {
        "x": 148.35000000000002,
        "y": 103.95
      },
      "event_ct50_eur1_ct50taueur1_ct50taueur1": {
        "x": 789.75,
        "y": 53.85000000000001
      },
      "event_Insert_Coffee_ct50_ct50": {
        "x": 309.75,
        "y": 190.35
      },
      "Insert": {
        "x": 789.75,
        "y": 215.55
      },
      "Chocolate": {
        "x": 1146.15,
        "y": 3.75
      },
      "event_Chocolate_Insert_GetChoc_GetChoc": {
        "x": 1327.9499999999998,
        "y": 202.65
      },
      "event_Coffee_Insert_GetCoffee_GetCoffee": {
        "x": 603.15,
        "y": 178.35
      },
      "Coffee": {
        "x": 451.95000000000005,
        "y": 178.35
      },
      "event_Insert_Chocolate_eur1_eur1": {
        "x": 967.3499999999999,
        "y": 127.95
      },
      "event_eur1_eur1_eur1taueur1_eur1taueur1": {
        "x": 1146.15,
        "y": 127.95
      },
      "event_eur1_ct50_eur1tauct50_eur1tauct50": {
        "x": 1146.15,
        "y": 276.75
      }
    },
    "edges": {},
    "weights": {}
  },
  "cyLayout_name_Simple": {
    "nodes": {
      "s0": {
        "x": 53.90205128205128,
        "y": 87.31653846153843
      },
      "event_a_a_offA_offA": {
        "x": 144.81538461538463,
        "y": 126.70012820512817
      },
      "s1": {
        "x": 247.1548717948718,
        "y": 95.07807692307692
      },
      "event_s1_s0_b_b": {
        "x": 148.05641025641026,
        "y": 19.840128205128206
      },
      "event_s0_s1_a_a": {
        "x": 141.57435897435897,
        "y": 178.90987179487175
      }
    },
    "edges": {}
  },
  "cyLayout_-866985705": {
    "nodes": {
      "event_x_z_xz_xz": {
        "x": 81.5746596858639,
        "y": 62.479895287958136
      },
      "event_z_y_zy_zy": {
        "x": 431.2619895287958,
        "y": 128.924502617801
      },
      "event_z_v_vz_vz": {
        "x": 486.77738219895286,
        "y": 269.3647120418848
      },
      "event_x_y_xy_xy": {
        "x": 282.65675392670175,
        "y": -86.33183246073291
      },
      "event_x_x_xx_xx": {
        "x": 59.56712041884825,
        "y": -214.21602094240833
      },
      "event_y_y_yy_yy": {
        "x": 586.645445026178,
        "y": -142.86356020942407
      },
      "v": {
        "x": 647.7421989528794,
        "y": 154.22167539267014
      },
      "event_y_x_yx_yx": {
        "x": 288.50722513089005,
        "y": -195.22712041884816
      },
      "event_z_z_zz_zz": {
        "x": 177.33047120418848,
        "y": 369.8956020942408
      },
      "z": {
        "x": 257.96041884816754,
        "y": 250.40073298429323
      },
      "x": {
        "x": 78.94136125654452,
        "y": -148.7496335078534
      },
      "y": {
        "x": 480.1679581151832,
        "y": -117.81057591623035
      }
    },
    "edges": {}
  },
  "cyLayout_name_EX1": {
    "nodes": {
      "w2": {
        "x": 383.5197253315035,
        "y": -136.15044005044822
      },
      "w1": {
        "x": 217.89999999999998,
        "y": 102.15
      },
      "w3": {
        "x": 434.7007297536677,
        "y": 407.3206375235881
      },
      "event_w3_w1_w31_w31": {
        "x": 484.2629641188392,
        "y": 226.58703588116072
      },
      "event_w3_w3_tau_deadlock": {
        "x": 716.3,
        "y": 109.15
      },
      "event_w2_w2_tau_deadlock": {
        "x": 116.30000000000001,
        "y": 35.95000000000002
      },
      "event_w1_w2_w12_w12": {
        "x": 415.7338913456467,
        "y": -24.738388123693554
      },
      "event_w1_w3_w13_w13": {
        "x": 241.98524358817969,
        "y": 324.67344927345624
      },
      "event_w13_w12_offW_offW": {
        "x": 401.595450225854,
        "y": 122.03491527300173
      },
      "event_w2_w1_w21_w21": {
        "x": 214.88568363862788,
        "y": -43.59872293108162
      },
      "event_w12_w21_onW_onW": {
        "x": 313.1170955676079,
        "y": -33.402558830497355
      },
      "event_w13_w31_onW2_onW2": {
        "x": 385.4078879343537,
        "y": 293.92290195820544
      }
    },
    "edges": {
      "rule_from_event_w1_w2_w12_w12_event_w12_w21_onW_onW": {
        "distances": [
          -2.715250421996503
        ],
        "weights": [
          0.6059452316184102
        ]
      },
      "rule_to_event_w12_w21_onW_onW_event_w2_w1_w21_w21": {
        "distances": [
          -2.8377136361688233
        ],
        "weights": [
          0.48474266644135405
        ]
      }
    }
  },
  "cyLayout_name_sala_inteligente": {
    "nodes": {
      "ocupado": {
        "x": -61.19999999999993,
        "y": 83.05769633507855
      },
      "ocioso": {
        "x": 183.35685863874346,
        "y": -7.810994764397924
      },
      "event_presenca_ruido_presencatauruido_presencatauruido": {
        "x": 415.32523560209415,
        "y": 327.6310994764398
      },
      "event_ocioso_ocupado_presenca_presenca": {
        "x": 150.175497382199,
        "y": 252.8725654450261
      },
      "event_ocupado_ocioso_timeout_timeout": {
        "x": 36.880209424083844,
        "y": -13.93874345549737
      },
      "event_ocioso_ocioso_ruido_ruido": {
        "x": 438.5438743455498,
        "y": 51.88240837696337
      },
      "event_ruido_presenca_ruidotaupresenca_ruidotaupresenca": {
        "x": 253.41905759162313,
        "y": 131.72115183246072
      }
    },
    "edges": {}
  },
  "cyLayout_name_Penguim": {
    "nodes": {
      "Bird": {
        "x": 683.55,
        "y": 15.775000000000006
      },
      "Son_of_Tweetie": {
        "x": -55.049999999999955,
        "y": 76.975
      },
      "Does_Fly": {
        "x": 923.6838709677419,
        "y": 34.445564516129025
      },
      "event_Penguim_noFly_PenguimtaunoFly_PenguimtaunoFly": {
        "x": 410.9637096774193,
        "y": 151.80766129032259
      },
      "event_Bird_Fly_noFly_noFly": {
        "x": 577.9596774193549,
        "y": 135.40806451612903
      },
      "event_Bird_Does_Fly_Fly_Fly": {
        "x": 806.55,
        "y": 76.975
      },
      "event_Special_Penguin_Penguin_Penguim_Penguim": {
        "x": 314.55,
        "y": 76.975
      },
      "Special_Penguin": {
        "x": 191.55,
        "y": 76.975
      },
      "event_Son_of_Tweetie_Special_Penguin_-_-": {
        "x": 68.55000000000001,
        "y": 76.975
      },
      "Penguin": {
        "x": 437.55,
        "y": 27.775
      },
      "event_Penguin_Bird_Bird_Bird": {
        "x": 560.55,
        "y": 27.775
      }
    },
    "edges": {}
  },
  "cyLayout_1635850785": {
    "nodes": {
      "event_Feed_List_watchLike_watchLike": {
        "x": -35.599999999999966,
        "y": 207.125
      },
      "event_Watch_Feed_dontLike_dontLike": {
        "x": 407.9828272251309,
        "y": 23.974528795811512
      },
      "event_Feed_Watch_watch_watch": {
        "x": 332.40167539267014,
        "y": 176.43767015706808
      },
      "Watch": {
        "x": 532.1652356020943,
        "y": 117.73536649214664
      },
      "event_List_Watch_watch2_watch2": {
        "x": 356.500942408377,
        "y": 371.71316753926703
      },
      "Feed": {
        "x": 169.72816753926716,
        "y": -32.087460732984276
      },
      "List": {
        "x": 172.34240837696333,
        "y": 395.4970418848168
      },
      "event_Watch_Feed_refresh_refresh": {
        "x": 492.07581151832454,
        "y": -21.761178010471202
      },
      "event_Watch_Watch_like_like": {
        "x": 656.8640837696335,
        "y": 187.78018324607336
      }
    },
    "edges": {}
  },
  "cyLayout_name_Moeda_Viciada_Tempo": {
    "nodes": {
      "lancar": {
        "x": 3.3499999999999943,
        "y": 71
      },
      "event_lancar_lancar_cara_cara": {
        "x": 3.5500000000000114,
        "y": -28.799999999999997
      },
      "event_lancar_lancar_coroa_coroa": {
        "x": 128.55,
        "y": 73.8
      },
      "event_cara_cara_viciar_viciar": {
        "x": 144.14999999999998,
        "y": -34.8
      }
    },
    "edges": {}
  },
  "cyLayout_name_Modelo_Automato": {
    "nodes": {
      "D": {
        "x": -225.71004510416867,
        "y": 194.32726443461743
      },
      "event_e1_e4_ativa_tracejado_topo_ativa_tracejado_topo": {
        "x": 212.05,
        "y": 175.425
      },
      "C2": {
        "x": 160.05,
        "y": 66.325
      },
      "event_e3_e5_ativa_tracejado_fim_ativa_tracejado_fim": {
        "x": 479.6907598231134,
        "y": 250.01411787736123
      },
      "C1": {
        "x": 212.05,
        "y": 460.4420208464378
      },
      "event_D_C2_e4_e4": {
        "x": 87.58579090343034,
        "y": 140.81857157298157
      },
      "F": {
        "x": 616.7489053694985,
        "y": 304.436322173087
      },
      "Faux": {
        "x": 570.5986477574671,
        "y": -17.953890334775625
      },
      "event_D_C1_e1_e1": {
        "x": -88.77191459250646,
        "y": 372.4061547693931
      },
      "event_C2_Faux_e5_e5": {
        "x": 416.3397724574144,
        "y": 24.125364876833828
      },
      "event_D_C1_e2_e2": {
        "x": -154.4466409348811,
        "y": 499.01448275419506
      },
      "event_e2_e4_ativa_tracejado_meio_ativa_tracejado_meio": {
        "x": 93.02582097287613,
        "y": 141.9472794693403
      },
      "event_C1_F_e3_e3": {
        "x": 480.52399658870695,
        "y": 399.1265346809497
      }
    },
    "edges": {}
  },
  "cyLayout_name_Conditions": {
    "nodes": {
      "start": {
        "x": -64.64384615384611,
        "y": 24.189230769230768
      },
      "event_middle_endN_activateStep2_activateStep2": {
        "x": 432.8461538461538,
        "y": 27
      },
      "middle": {
        "x": 304.22461538461533,
        "y": 25.594615384615384
      },
      "endN": {
        "x": 527.7384615384614,
        "y": 25.594615384615384
      },
      "event_start_middle_step1_step1": {
        "x": 109.55000000000001,
        "y": 27
      }
    },
    "edges": {}
  },
  "cyLayout_-1337987662": {
    "nodes": {
      "s0": {
        "x": -3.799999999999983,
        "y": 60.325
      },
      "s1": {
        "x": 281.8,
        "y": 83.42500000000001
      },
      "event_s0_s1_a_a": {
        "x": 152.8,
        "y": 198.425
      },
      "event_s1_s0_b_b": {
        "x": 162.79999999999995,
        "y": 23.325000000000003
      }
    },
    "edges": {}
  },
  "cyLayout_name_CommittedEmulation": {
    "nodes": {
      "event_D_F_df_df": {
        "x": 24.64428057989767,
        "y": -86.34829432872162
      },
      "C2": {
        "x": 180.25413800270945,
        "y": 321.7300002653275
      },
      "event_C1_D_c1d2_back2": {
        "x": -90.44135014177502,
        "y": 102.58694109598115
      },
      "event_back1_bf_back1taubf_back1taubf": {
        "x": -179.38336234600447,
        "y": -49.859557546455655
      },
      "event_back4_df_back4taudf_back4taudf": {
        "x": -19.99307644920807,
        "y": 138.87171353894078
      },
      "event_back4_dfaux_back4taudfaux_back4taudfaux": {
        "x": -101.77399298684665,
        "y": 372.09747033503754
      },
      "event_F_C1_dfc1_enterC1": {
        "x": 367.13113535143196,
        "y": -1.0042774583905465
      },
      "event_C1_D_c1d1_back1": {
        "x": -153.18599454141108,
        "y": 22.84595247937832
      },
      "event_C2_D_dc21_back3": {
        "x": -75.03927082480546,
        "y": 215.7183921727518
      },
      "F": {
        "x": 350.7903680620464,
        "y": -131.88035270994712
      },
      "Faux": {
        "x": 38.81091183361558,
        "y": 510.3603288430475
      },
      "event_back2_dfaux_bfaux_bfaux": {
        "x": -191.49425875856966,
        "y": 264.10196426546577
      },
      "event_back2_df_bf_bf": {
        "x": 96.06204699760757,
        "y": 50.87068604356998
      },
      "event_Faux_C2_c2fx_enterC2": {
        "x": 179.80602911556588,
        "y": 448.3088611033785
      },
      "event_D_Faux_dfaux_dfaux": {
        "x": -183.94281170160193,
        "y": 434.13658483739715
      },
      "C1": {
        "x": 363.9449899371471,
        "y": 104.63354128169918
      },
      "event_back2_bf_back2taubf_back2taubf": {
        "x": 84.94991542257495,
        "y": -199.9190254798413
      },
      "event_C2_D_dc22_back4": {
        "x": -88.1074624473771,
        "y": 294.90689796640197
      },
      "D": {
        "x": -506.3673265358391,
        "y": 91.62187585928407
      },
      "event_back2_bfaux_back2taubfaux_back2taubfaux": {
        "x": -330.53450485897747,
        "y": 196.90032839419953
      }
    },
    "edges": {
      "a_to_s_event_F_C1_dfc1_enterC1_C1": {
        "distances": [
          1.8518782473422934
        ],
        "weights": [
          0.53932464511797
        ]
      },
      "rule_from_event_C2_D_dc22_back4_event_back4_df_back4taudf_back4taudf": {
        "distances": [
          -184.0574164219715
        ],
        "weights": [
          0.6019877221951807
        ]
      },
      "rule_from_event_C1_D_c1d1_back1_event_back1_bf_back1taubf_back1taubf": {
        "distances": [
          -32.61483677589917
        ],
        "weights": [
          0.7781502453475913
        ]
      },
      "rule_to_event_back4_dfaux_back4taudfaux_back4taudfaux_event_D_Faux_dfaux_dfaux": {
        "distances": [
          -32.5952578961812
        ],
        "weights": [
          0.3726469132928407
        ]
      },
      "rule_from_event_C2_D_dc22_back4_event_back4_dfaux_back4taudfaux_back4taudfaux": {
        "distances": [
          -17.470161263028015
        ],
        "weights": [
          0.5566351277632117
        ]
      },
      "rule_to_event_back1_bf_back1taubf_back1taubf_event_back2_df_bf_bf": {
        "distances": [
          -30.80152452072727
        ],
        "weights": [
          0.4936779583504966
        ]
      },
      "s_to_a_F_event_F_C1_dfc1_enterC1": {
        "distances": [
          0.5669030871857552
        ],
        "weights": [
          0.6002067070473857
        ]
      },
      "s_to_a_C2_event_C2_D_dc22_back4": {
        "distances": [
          20.91426236842209
        ],
        "weights": [
          0.5101042264090844
        ]
      }
    }
  },
  "cyLayout_name_EX2": {
    "nodes": {
      "w2": {
        "x": 354.54137672587603,
        "y": -67.80587002261225
      },
      "w3": {
        "x": 444.7097705456873,
        "y": 438.99707460769366
      },
      "event_w13_w12_offW_offW": {
        "x": 258.343813584788,
        "y": 280.9773357771894
      },
      "event_w1_w3_w13_w13": {
        "x": 190.1472275104887,
        "y": 378.4524474459217
      },
      "event_w13_w31_onW2_onW2": {
        "x": 331.965468427505,
        "y": 343.49022945431267
      },
      "event_w1_w2_w12_w12": {
        "x": 354.5882336311924,
        "y": 101.33504726659318
      },
      "event_w12_w21_onW_onW": {
        "x": 238.0552622178189,
        "y": 38.60598763437099
      },
      "event_w3_w1_w31_w31": {
        "x": 441.98403438811073,
        "y": 295.32703633193483
      },
      "w1": {
        "x": 168.13696939941698,
        "y": 193.43615679044302
      },
      "event_w2_w1_w21_w21": {
        "x": 122.57865061127606,
        "y": 9.161308009994578
      }
    },
    "edges": {
      "rule_from_event_w1_w2_w12_w12_event_w12_w21_onW_onW": {
        "distances": [
          4.650618708840582
        ],
        "weights": [
          0.4734129264221702
        ]
      },
      "rule_to_event_w12_w21_onW_onW_event_w2_w1_w21_w21": {
        "distances": [
          -5.3433453775118736
        ],
        "weights": [
          0.5130563633882637
        ]
      }
    }
  },
  "cyLayout_name_Coin": {
    "nodes": {
      "event_toss2_tossHeads2_c4_c4": {
        "x": 148.4277205306597,
        "y": 210.0263522425329
      },
      "event_start_heads_toss_toss2": {
        "x": 236.60078742028892,
        "y": 318.986442225606
      },
      "event_toss2_tossHeads1_c3_c3": {
        "x": 450.2842562224805,
        "y": 292.86141339229556
      },
      "heads": {
        "x": 271.6970444551262,
        "y": 123.37188239311703
      },
      "event_heads_heads_toss_tossHeads2": {
        "x": 228.8252419131947,
        "y": 75.87596253336051
      },
      "start": {
        "x": 454.9444659113156,
        "y": 385.7927907597333
      },
      "tails": {
        "x": 627.0171281112404,
        "y": 138.7225531269657
      },
      "event_heads_tails_toss_tossTails": {
        "x": 491.1525833507508,
        "y": 55.96614406869335
      },
      "event_tossHeads1_tossHeads1_c1_c1": {
        "x": 487.36045255602943,
        "y": 116.5139439414945
      },
      "event_start_tails_toss_toss1": {
        "x": 635.7115046115039,
        "y": 327.9886617813189
      },
      "event_tossHeads2_tossHeads2_c2_c2": {
        "x": 186.3431315225334,
        "y": 9.248191661424887
      },
      "event_tails_tails_toss_tossTails": {
        "x": 691.6608821226387,
        "y": 115.92481756158308
      },
      "event_tails_heads_toss_tossHeads1": {
        "x": 493.2872504107655,
        "y": 195.47799418828507
      }
    },
    "edges": {
      "a_to_s_event_start_heads_toss_toss2_heads": {
        "distances": [
          -83.01321320963196
        ],
        "weights": [
          0.583342002713638
        ]
      },
      "rule_to_event_toss2_tossHeads2_c4_c4_event_heads_heads_toss_tossHeads2": {
        "distances": [
          -75.9236832318482
        ],
        "weights": [
          0.5290586698066435
        ]
      },
      "a_to_s_event_tails_heads_toss_tossHeads1_heads": {
        "distances": [
          -40.95179343278694
        ],
        "weights": [
          0.3676121880757556
        ]
      },
      "a_to_s_event_heads_tails_toss_tossTails_tails": {
        "distances": [
          -74.53962619020056
        ],
        "weights": [
          0.45889594167055076
        ]
      },
      "s_to_a_start_event_start_heads_toss_toss2": {
        "distances": [
          -28.18854300549969
        ],
        "weights": [
          0.48390872583222083
        ]
      },
      "rule_from_event_start_heads_toss_toss2_event_toss2_tossHeads2_c4_c4": {
        "distances": [
          -48.7375785359
        ],
        "weights": [
          0.6021713484310195
        ]
      },
      "s_to_a_heads_event_heads_tails_toss_tossTails": {
        "distances": [
          -81.81253760353643,
          -87.24216604282394
        ],
        "weights": [
          0.5240807972190754,
          0.5889495447416486
        ]
      },
      "s_to_a_tails_event_tails_heads_toss_tossHeads1": {
        "distances": [
          -39.78143415400139
        ],
        "weights": [
          0.46480102300318504
        ]
      }
    }
  },
  "cyLayout_name_NN": {
    "nodes": {
      "event_yx_xz_yxxz_yxxz": {
        "x": 403.3576434856501,
        "y": -69.87195182149617
      },
      "event_x_x_xx_xx": {
        "x": 52.86282722513095,
        "y": -222.66439790575913
      },
      "event_y_y_yy_yy": {
        "x": 839.6253403141362,
        "y": -206.20827225130887
      },
      "event_x_z_xz_xz": {
        "x": 194.12335078534028,
        "y": 33.96806282722514
      },
      "event_y_x_yx_yx": {
        "x": 444.95853403141365,
        "y": -228.0604188481675
      },
      "event_z_y_zy_zy": {
        "x": 684.6075392670158,
        "y": 74.40743455497385
      },
      "z": {
        "x": 450.6341361256544,
        "y": 239.38670157068063
      },
      "x": {
        "x": 141.61738219895292,
        "y": -164.15801047120416
      },
      "y": {
        "x": 791.2312041884817,
        "y": -124.53685863874344
      },
      "event_z_z_zz_zz": {
        "x": 488.5864141882075,
        "y": 129.04208658753868
      }
    },
    "edges": {}
  },
  "cyLayout_name_HabitLearner": {
    "nodes": {
      "event_Descanso_Inicio_terminar_terminar": {
        "x": 293.2796690307331,
        "y": -34.95283687943257
      },
      "event_Inicio_Trabalho_e_trabalho_e_trabalho": {
        "x": 246.90236406619377,
        "y": -176.36134751773045
      },
      "Inicio": {
        "x": -17.94444444444442,
        "y": 86.56773049645389
      },
      "Lazer": {
        "x": 445.0877068557919,
        "y": 558.7228132387708
      },
      "event_Lazer_Inicio_terminar_terminar": {
        "x": 602.3,
        "y": 356.45
      },
      "Trabalho": {
        "x": 438.4815602836879,
        "y": -186.2705673758865
      },
      "event_Inicio_Lazer_e_lazer_e_lazer": {
        "x": 261.7661938534279,
        "y": 390.2660756501182
      },
      "event_Inicio_Descanso_e_descanso_e_descanso": {
        "x": 400.4952718676123,
        "y": 132.63770685579195
      },
      "Descanso": {
        "x": 483.07304964539003,
        "y": 26.154018912529622
      },
      "event_Trabalho_Inicio_terminar_terminar": {
        "x": 126.65744680851061,
        "y": -322.481914893617
      }
    },
    "edges": {}
  },
  "cyLayout_name_Emulation": {
    "nodes": {
      "event_P10_P11_t_p10_p11_t_p10_p11": {
        "x": 2242,
        "y": -278
      },
      "event_back4_df_back4taudf_back4taudf": {
        "x": 1323,
        "y": -567
      },
      "event_back4_dfaux_back4taudfaux_back4taudfaux": {
        "x": 1545,
        "y": -289
      },
      "P61": {
        "x": 3046,
        "y": -582
      },
      "event_P4_P5_t_p4_p5_t_p4_p5": {
        "x": 3111,
        "y": -786
      },
      "P8a": {
        "x": 2816,
        "y": -497
      },
      "P11": {
        "x": 2245,
        "y": -196
      },
      "P4": {
        "x": 2978,
        "y": -782
      },
      "event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1": {
        "x": 3104,
        "y": -444
      },
      "event_P3_P31_t_p3_p31_t_p3_p31": {
        "x": 2558,
        "y": -726
      },
      "P9a1": {
        "x": 2797,
        "y": -211
      },
      "P22": {
        "x": 2355,
        "y": -680
      },
      "event_P22_P7_t_p22_p7_t_p22_p7": {
        "x": 2551,
        "y": -635
      },
      "C2": {
        "x": 1778,
        "y": -105
      },
      "event_D_P1_df_df": {
        "x": 1530,
        "y": -764
      },
      "event_P16_F_to_F_to_F": {
        "x": 2035,
        "y": -100
      },
      "event_P9a2_P11_t_p9a2_p11_t_p9a2_p11": {
        "x": 2349,
        "y": -241
      },
      "event_P14_P15_t_p14_p15_t_p14_p15": {
        "x": 2992,
        "y": -69
      },
      "P9a2": {
        "x": 2490,
        "y": -276
      },
      "event_C1_D_c1d2_back2": {
        "x": 1665,
        "y": -574
      },
      "event_P3_P32_t_p3_p32_t_p3_p32": {
        "x": 2613,
        "y": -784
      },
      "event_back1_bf_back1taubf_back1taubf": {
        "x": 1790,
        "y": -671
      },
      "P7a2": {
        "x": 3121,
        "y": -247
      },
      "event_D_P1_dfaux_dfaux": {
        "x": 1866,
        "y": -328
      },
      "P9": {
        "x": 2240,
        "y": -508
      },
      "event_P31_P4a_t_p31_p4a_t_p31_p4a": {
        "x": 2819,
        "y": -711
      },
      "event_P6_P61_t_p6_p61_t_p6_p61": {
        "x": 3144,
        "y": -579
      },
      "event_P2_P22_t_p2_p22_t_p2_p22": {
        "x": 2140,
        "y": -686
      },
      "event_P13_P14_t_p13_p14_t_p13_p14": {
        "x": 2707,
        "y": -58
      },
      "event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2": {
        "x": 3092,
        "y": -310
      },
      "P9a": {
        "x": 2823,
        "y": -348
      },
      "D": {
        "x": 1474,
        "y": -511
      },
      "event_F_C1_dfc1_enterC1": {
        "x": 2043,
        "y": -430
      },
      "event_P8_P9_t_p8_p9_t_p8_p9": {
        "x": 2303,
        "y": -585
      },
      "event_P62_P7a_t_p62_p7a_t_p62_p7a": {
        "x": 3247,
        "y": -381
      },
      "P6": {
        "x": 3251,
        "y": -596
      },
      "event_P1_P2_t_p1_p2_t_p1_p2": {
        "x": 1874,
        "y": -800
      },
      "event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1": {
        "x": 2804,
        "y": -287
      },
      "P5": {
        "x": 3236,
        "y": -796
      },
      "event_P4a_P5_t_p4a_p5_t_p4a_p5": {
        "x": 3126,
        "y": -726
      },
      "P4a": {
        "x": 2981,
        "y": -713
      },
      "F": {
        "x": 2064,
        "y": -337
      },
      "event_P15_P16_t_p15_p16_t_p15_p16": {
        "x": 2615,
        "y": 40
      },
      "P10": {
        "x": 2244,
        "y": -360
      },
      "C1": {
        "x": 1973,
        "y": -541
      },
      "event_back2_bf_back2taubf_back2taubf": {
        "x": 1714,
        "y": -634
      },
      "event_P9_P10_t_p9_p10_t_p9_p10": {
        "x": 2260,
        "y": -431
      },
      "P62": {
        "x": 3247,
        "y": -451
      },
      "event_C1_D_c1d1_back1": {
        "x": 1762,
        "y": -489
      },
      "P13": {
        "x": 2585,
        "y": -75
      },
      "P1": {
        "x": 1876,
        "y": -669
      },
      "event_P2_P21_t_p2_p21_t_p2_p21": {
        "x": 2114,
        "y": -863
      },
      "event_P9a1_P14_t_p9a1_p14_t_p9a1_p14": {
        "x": 2799,
        "y": -136
      },
      "event_C2_D_dc21_back3": {
        "x": 1685,
        "y": -230
      },
      "P31": {
        "x": 2675,
        "y": -704
      },
      "event_back1_bfaux_back1taubfaux_back1taubfaux": {
        "x": 1839,
        "y": -425
      },
      "event_P32_P4_t_p32_p4_t_p32_p4": {
        "x": 2839,
        "y": -782
      },
      "event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2": {
        "x": 2625,
        "y": -322
      },
      "event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a": {
        "x": 2949,
        "y": -499
      },
      "P7a1": {
        "x": 3108,
        "y": -510
      },
      "event_P12_P13_t_p12_p13_t_p12_p13": {
        "x": 2463,
        "y": -82
      },
      "event_P61_P7_t_p61_p7_t_p61_p7": {
        "x": 2911,
        "y": -581
      },
      "event_back2_bfaux_back2taubfaux_back2taubfaux": {
        "x": 1624,
        "y": -468
      },
      "Faux": {
        "x": 1799,
        "y": 125
      },
      "event_P5_P6_t_p5_p6_t_p5_p6": {
        "x": 3246,
        "y": -680
      },
      "event_C2_D_dc22_back4": {
        "x": 1453,
        "y": -193
      },
      "event_P16_Faux_to_Faux_to_Faux": {
        "x": 1945,
        "y": 128
      },
      "P21": {
        "x": 2233,
        "y": -787
      },
      "event_P21_P3_t_p21_p3_t_p21_p3": {
        "x": 2376,
        "y": -786
      },
      "event_P11_P12_t_p11_p12_t_p11_p12": {
        "x": 2197,
        "y": -98
      },
      "event_P6_P62_t_p6_p62_t_p6_p62": {
        "x": 3244,
        "y": -522
      },
      "event_back2_dfaux_bfaux_bfaux": {
        "x": 1733,
        "y": -426
      },
      "event_back2_df_bf_bf": {
        "x": 1614,
        "y": -693
      },
      "P8": {
        "x": 2432,
        "y": -592
      },
      "event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa": {
        "x": 3106,
        "y": -175
      },
      "P16": {
        "x": 2189,
        "y": 34
      },
      "P12": {
        "x": 2332,
        "y": -94
      },
      "event_Faux_C2_c2fx_enterC2": {
        "x": 1800,
        "y": 23
      },
      "P3": {
        "x": 2501,
        "y": -783
      },
      "P15": {
        "x": 2944,
        "y": 42
      },
      "P2": {
        "x": 2052,
        "y": -798
      },
      "P7": {
        "x": 2762,
        "y": -585
      },
      "event_P8a_P9a_t_p8a_p9a_t_p8a_p9a": {
        "x": 2812,
        "y": -420
      },
      "P8aa": {
        "x": 2935,
        "y": -214
      },
      "event_P7_P8_t_p7_p8_t_p7_p8": {
        "x": 2595,
        "y": -583
      },
      "P7a": {
        "x": 3095,
        "y": -375
      },
      "event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a": {
        "x": 2939,
        "y": -319
      },
      "P14": {
        "x": 2847,
        "y": -59
      },
      "P32": {
        "x": 2703,
        "y": -783
      }
    },
    "edges": {
      "s_to_a_P7a2_event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa": {
        "distances": [
          -13.051504030654995
        ],
        "weights": [
          0.4712548091660469
        ]
      },
      "a_to_s_event_P8a_P9a_t_p8a_p9a_t_p8a_p9a_P9a": {
        "distances": [
          6.214525527178343
        ],
        "weights": [
          0.456807342815261
        ]
      },
      "a_to_s_event_P16_F_to_F_to_F_F": {
        "distances": [
          -5.593769099510957
        ],
        "weights": [
          0.47073988335762845
        ]
      },
      "a_to_s_event_P2_P22_t_p2_p22_t_p2_p22_P22": {
        "distances": [
          8.071867193548387
        ],
        "weights": [
          0.47888102745660077
        ]
      },
      "s_to_a_P32_event_P32_P4_t_p32_p4_t_p32_p4": {
        "distances": [
          8.197944494114392
        ],
        "weights": [
          0.4635467123404698
        ]
      },
      "a_to_s_event_P6_P62_t_p6_p62_t_p6_p62_P62": {
        "distances": [
          7.217662948313478
        ],
        "weights": [
          0.41605982482431136
        ]
      },
      "a_to_s_event_P14_P15_t_p14_p15_t_p14_p15_P15": {
        "distances": [
          -10.096395904085002
        ],
        "weights": [
          0.6142733840810428
        ]
      },
      "a_to_s_event_F_C1_dfc1_enterC1_C1": {
        "distances": [
          9.294814422893037
        ],
        "weights": [
          0.5194094836930605
        ]
      },
      "rule_to_event_back4_df_back4taudf_back4taudf_event_D_P1_df_df": {
        "distances": [
          -73.87556916260674
        ],
        "weights": [
          0.36945723493778443
        ]
      },
      "s_to_a_P4_event_P4_P5_t_p4_p5_t_p4_p5": {
        "distances": [
          10.041249358516463
        ],
        "weights": [
          0.49873741842237856
        ]
      },
      "a_to_s_event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a_P9a": {
        "distances": [
          1.2953067079886995
        ],
        "weights": [
          0.5551622487911034
        ]
      },
      "s_to_a_P9a2_event_P9a2_P11_t_p9a2_p11_t_p9a2_p11": {
        "distances": [
          -1.2700568450644283
        ],
        "weights": [
          0.5064345219768094
        ]
      },
      "a_to_s_event_C2_D_dc21_back3_D": {
        "distances": [
          9.485128273603234
        ],
        "weights": [
          0.4971651602776708
        ]
      },
      "s_to_a_D_event_D_P1_df_df": {
        "distances": [
          -53.41820359240714
        ],
        "weights": [
          0.5918397470268428
        ]
      },
      "s_to_a_P7a_event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1": {
        "distances": [
          0.4273577127872325
        ],
        "weights": [
          0.38523319263796557
        ]
      },
      "a_to_s_event_P9a1_P14_t_p9a1_p14_t_p9a1_p14_P14": {
        "distances": [
          -1.1595086616994803
        ],
        "weights": [
          0.42522912541829955
        ]
      },
      "rule_from_event_C1_D_c1d1_back1_event_back1_bfaux_back1taubfaux_back1taubfaux": {
        "distances": [
          -62.52781533516959
        ],
        "weights": [
          0.6330990367674839
        ]
      },
      "a_to_s_event_P22_P7_t_p22_p7_t_p22_p7_P7": {
        "distances": [
          -0.07267180530930178
        ],
        "weights": [
          0.3360943077952167
        ]
      },
      "s_to_a_P9a_event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1": {
        "distances": [
          -0.2705180435622271
        ],
        "weights": [
          0.5088659786604878
        ]
      },
      "s_to_a_P5_event_P5_P6_t_p5_p6_t_p5_p6": {
        "distances": [
          -6.725267127561253
        ],
        "weights": [
          0.5623248316318856
        ]
      },
      "s_to_a_P13_event_P13_P14_t_p13_p14_t_p13_p14": {
        "distances": [
          1.7007298023608266
        ],
        "weights": [
          0.47612980981898606
        ]
      },
      "a_to_s_event_P32_P4_t_p32_p4_t_p32_p4_P4": {
        "distances": [
          10.036349414637755
        ],
        "weights": [
          0.3653831562060871
        ]
      },
      "s_to_a_P6_event_P6_P62_t_p6_p62_t_p6_p62": {
        "distances": [
          2.6324392988209344
        ],
        "weights": [
          0.5142256966983064
        ]
      },
      "s_to_a_P62_event_P62_P7a_t_p62_p7a_t_p62_p7a": {
        "distances": [
          7.794337209807774
        ],
        "weights": [
          0.5181061362397678
        ]
      },
      "rule_from_event_C2_D_dc22_back4_event_back4_df_back4taudf_back4taudf": {
        "distances": [
          -82.43167248287288
        ],
        "weights": [
          0.6378570688455208
        ]
      },
      "a_to_s_event_P15_P16_t_p15_p16_t_p15_p16_P16": {
        "distances": [
          -7.644861695207878
        ],
        "weights": [
          0.4663977409322675
        ]
      },
      "a_to_s_event_P9a2_P11_t_p9a2_p11_t_p9a2_p11_P11": {
        "distances": [
          -0.15079933015025793
        ],
        "weights": [
          0.4809647671985674
        ]
      },
      "s_to_a_P9a_event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2": {
        "distances": [
          15.737609481734737
        ],
        "weights": [
          0.5607526859648813
        ]
      },
      "s_to_a_P4a_event_P4a_P5_t_p4a_p5_t_p4a_p5": {
        "distances": [
          4.180528705457987
        ],
        "weights": [
          0.47672938147250843
        ]
      },
      "a_to_s_event_C1_D_c1d1_back1_D": {
        "distances": [
          12.324479872370038
        ],
        "weights": [
          0.5330624616233524
        ]
      },
      "a_to_s_event_P2_P21_t_p2_p21_t_p2_p21_P21": {
        "distances": [
          5.213098956432523
        ],
        "weights": [
          0.2939901220712554
        ]
      },
      "a_to_s_event_P13_P14_t_p13_p14_t_p13_p14_P14": {
        "distances": [
          0.5639937390058848
        ],
        "weights": [
          0.47861167732432935
        ]
      },
      "a_to_s_event_P62_P7a_t_p62_p7a_t_p62_p7a_P7a": {
        "distances": [
          -1.162986735578697
        ],
        "weights": [
          0.38158844590416585
        ]
      },
      "s_to_a_P9a1_event_P9a1_P14_t_p9a1_p14_t_p9a1_p14": {
        "distances": [
          -1.3717995477892382
        ],
        "weights": [
          0.4461242559820761
        ]
      },
      "s_to_a_C2_event_C2_D_dc22_back4": {
        "distances": [
          -38.07753367151218
        ],
        "weights": [
          0.4619886075402921
        ]
      },
      "s_to_a_P8a_event_P8a_P9a_t_p8a_p9a_t_p8a_p9a": {
        "distances": [
          -4.583418887191594
        ],
        "weights": [
          0.5644709409355415
        ]
      },
      "s_to_a_P14_event_P14_P15_t_p14_p15_t_p14_p15": {
        "distances": [
          5.490093033469452
        ],
        "weights": [
          0.5890304805022102
        ]
      },
      "s_to_a_P16_event_P16_Faux_to_Faux_to_Faux": {
        "distances": [
          -7.402392450998517
        ],
        "weights": [
          0.6116400166606928
        ]
      },
      "s_to_a_P8aa_event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a": {
        "distances": [
          -66.58216537798964
        ],
        "weights": [
          0.1878311859942721
        ]
      },
      "a_to_s_event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1_P7a1": {
        "distances": [
          -4.39745335044915
        ],
        "weights": [
          0.4808210086982788
        ]
      },
      "a_to_s_event_P7_P8_t_p7_p8_t_p7_p8_P8": {
        "distances": [
          2.3619150082195794
        ],
        "weights": [
          0.4926439822406519
        ]
      },
      "a_to_s_event_Faux_C2_c2fx_enterC2_C2": {
        "distances": [
          0.07207514581779101
        ],
        "weights": [
          0.5459569691796047
        ]
      },
      "a_to_s_event_P12_P13_t_p12_p13_t_p12_p13_P13": {
        "distances": [
          -1.8356376565485644
        ],
        "weights": [
          0.5387479207309417
        ]
      },
      "a_to_s_event_P4_P5_t_p4_p5_t_p4_p5_P5": {
        "distances": [
          14.441949185372922
        ],
        "weights": [
          0.41126476116482075
        ]
      },
      "a_to_s_event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2_P7a2": {
        "distances": [
          2.0012396417275036
        ],
        "weights": [
          0.4954630973515798
        ]
      },
      "s_to_a_P3_event_P3_P31_t_p3_p31_t_p3_p31": {
        "distances": [
          11.311540243511656
        ],
        "weights": [
          0.4564902602120284
        ]
      },
      "a_to_s_event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1_P9a1": {
        "distances": [
          2.1412225400799336
        ],
        "weights": [
          0.4415725308377675
        ]
      },
      "s_to_a_P11_event_P11_P12_t_p11_p12_t_p11_p12": {
        "distances": [
          4.3908431497079725
        ],
        "weights": [
          0.5267381092353445
        ]
      },
      "s_to_a_P7_event_P7_P8_t_p7_p8_t_p7_p8": {
        "distances": [
          -1.0211071844502946
        ],
        "weights": [
          0.6626217687755693
        ]
      },
      "s_to_a_P16_event_P16_F_to_F_to_F": {
        "distances": [
          -19.76764227759268
        ],
        "weights": [
          0.5974439233894271
        ]
      },
      "rule_from_event_C2_D_dc22_back4_event_back4_dfaux_back4taudfaux_back4taudfaux": {
        "distances": [
          -8.749166593073433
        ],
        "weights": [
          0.530590987590989
        ]
      },
      "a_to_s_event_P16_Faux_to_Faux_to_Faux_Faux": {
        "distances": [
          -26.08408381504541
        ],
        "weights": [
          0.6842604320331206
        ]
      },
      "a_to_s_event_P6_P61_t_p6_p61_t_p6_p61_P61": {
        "distances": [
          -0.25648077889113036
        ],
        "weights": [
          0.5276527534306457
        ]
      },
      "rule_to_event_back1_bfaux_back1taubfaux_back1taubfaux_event_back2_dfaux_bfaux_bfaux": {
        "distances": [
          -52.380201707046375
        ],
        "weights": [
          0.4319622071314717
        ]
      },
      "s_to_a_P15_event_P15_P16_t_p15_p16_t_p15_p16": {
        "distances": [
          9.697305674959566
        ],
        "weights": [
          0.4937415734057811
        ]
      },
      "a_to_s_event_P1_P2_t_p1_p2_t_p1_p2_P2": {
        "distances": [
          2.330573824615695
        ],
        "weights": [
          0.4276050235232787
        ]
      },
      "s_to_a_P3_event_P3_P32_t_p3_p32_t_p3_p32": {
        "distances": [
          4.767444352714022
        ],
        "weights": [
          0.23461060023521832
        ]
      },
      "s_to_a_P61_event_P61_P7_t_p61_p7_t_p61_p7": {
        "distances": [
          -7.028577438119633
        ],
        "weights": [
          0.5586561034084216
        ]
      },
      "s_to_a_P7a1_event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a": {
        "distances": [
          1.3156589442962614
        ],
        "weights": [
          0.5974134704872403
        ]
      },
      "a_to_s_event_P3_P31_t_p3_p31_t_p3_p31_P31": {
        "distances": [
          9.290530738309215
        ],
        "weights": [
          0.46592246288821
        ]
      },
      "s_to_a_C2_event_C2_D_dc21_back3": {
        "distances": [
          -4.540240166797482
        ],
        "weights": [
          0.5635705601829354
        ]
      },
      "rule_to_event_back4_dfaux_back4taudfaux_back4taudfaux_event_D_P1_dfaux_dfaux": {
        "distances": [
          -1.323170524811627
        ],
        "weights": [
          0.5139018201362755
        ]
      },
      "a_to_s_event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a_P8a": {
        "distances": [
          4.774095778940595
        ],
        "weights": [
          0.5129792627625116
        ]
      },
      "s_to_a_P22_event_P22_P7_t_p22_p7_t_p22_p7": {
        "distances": [
          7.3609749950245735
        ],
        "weights": [
          0.44531876858220754
        ]
      },
      "a_to_s_event_P3_P32_t_p3_p32_t_p3_p32_P32": {
        "distances": [
          3.747914723197855
        ],
        "weights": [
          0.34828419137591976
        ]
      },
      "a_to_s_event_P61_P7_t_p61_p7_t_p61_p7_P7": {
        "distances": [
          -2.0391528288318215
        ],
        "weights": [
          0.525333206429449
        ]
      },
      "s_to_a_F_event_F_C1_dfc1_enterC1": {
        "distances": [
          1.8050728912367908
        ],
        "weights": [
          0.6911968535391935
        ]
      },
      "s_to_a_P12_event_P12_P13_t_p12_p13_t_p12_p13": {
        "distances": [
          -22.80064702983357
        ],
        "weights": [
          0.44857234819179376
        ]
      },
      "a_to_s_event_P11_P12_t_p11_p12_t_p11_p12_P12": {
        "distances": [
          -16.175448616796604
        ],
        "weights": [
          0.4730902276989925
        ]
      },
      "a_to_s_event_P5_P6_t_p5_p6_t_p5_p6_P6": {
        "distances": [
          4.276088516717775
        ],
        "weights": [
          0.41551460405800417
        ]
      },
      "a_to_s_event_P31_P4a_t_p31_p4a_t_p31_p4a_P4a": {
        "distances": [
          6.019454882109193
        ],
        "weights": [
          0.44023577303144223
        ]
      },
      "s_to_a_P7a_event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2": {
        "distances": [
          -3.44560638646909
        ],
        "weights": [
          0.5715695580888575
        ]
      },
      "s_to_a_C1_event_C1_D_c1d1_back1": {
        "distances": [
          3.763241057746482
        ],
        "weights": [
          0.5719445552669763
        ]
      },
      "a_to_s_event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2_P9a2": {
        "distances": [
          9.169074820280535
        ],
        "weights": [
          0.45086813575244483
        ]
      },
      "s_to_a_P31_event_P31_P4a_t_p31_p4a_t_p31_p4a": {
        "distances": [
          0.7133436324947633
        ],
        "weights": [
          0.4822373295479973
        ]
      },
      "a_to_s_event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa_P8aa": {
        "distances": [
          0.13036664069581394
        ],
        "weights": [
          0.500410050725852
        ]
      },
      "a_to_s_event_P21_P3_t_p21_p3_t_p21_p3_P3": {
        "distances": [
          15.38664074555459
        ],
        "weights": [
          0.5290372597102201
        ]
      },
      "s_to_a_P21_event_P21_P3_t_p21_p3_t_p21_p3": {
        "distances": [
          4.31366582430282
        ],
        "weights": [
          0.46846570311932184
        ]
      },
      "a_to_s_event_C2_D_dc22_back4_D": {
        "distances": [
          5.773490308975977
        ],
        "weights": [
          0.5417794341971569
        ]
      },
      "s_to_a_Faux_event_Faux_C2_c2fx_enterC2": {
        "distances": [
          0.855237941997911
        ],
        "weights": [
          0.5636662284184406
        ]
      },
      "s_to_a_P6_event_P6_P61_t_p6_p61_t_p6_p61": {
        "distances": [
          -4.1398152897448774
        ],
        "weights": [
          0.5081207723618868
        ]
      },
      "s_to_a_P8_event_P8_P9_t_p8_p9_t_p8_p9": {
        "distances": [
          -9.038325041862995
        ],
        "weights": [
          0.5432268710586388
        ]
      },
      "a_to_s_event_D_P1_df_df_P1": {
        "distances": [
          -74.83478371477828
        ],
        "weights": [
          0.3581576142141127
        ]
      },
      "s_to_a_P1_event_P1_P2_t_p1_p2_t_p1_p2": {
        "distances": [
          1.8148404659046362
        ],
        "weights": [
          0.5370247310463049
        ]
      },
      "s_to_a_P2_event_P2_P21_t_p2_p21_t_p2_p21": {
        "distances": [
          -3.202313426437055
        ],
        "weights": [
          0.34133922244306747
        ]
      }
    }
  },
  "cyLayout_name_BPI_2012_Automated": {
    "nodes": {
      "event_A_PARTLYSUBMITTED_W_Afhandelen_leads_W_Afhandelen_leads_e3": {
        "x": 612.8624161073825,
        "y": 390.78299776286354
      },
      "event_W_Completeren_aanvraag_W_Completeren_aanvraag_W_Completeren_aanvraag_e9": {
        "x": 1745.8581166976107,
        "y": 409.5964564008079
      },
      "A_SUBMITTED": {
        "x": -59.639821029082775,
        "y": 382.68680089485457
      },
      "event_A_PARTLYSUBMITTED_A_DECLINED_A_DECLINED_e1": {
        "x": 707.3769574944073,
        "y": 96.46756152125279
      },
      "event_A_PREACCEPTED_W_Completeren_aanvraag_W_Completeren_aanvraag_e15": {
        "x": 1257.6298228200403,
        "y": 718.3581166976107
      },
      "A_CANCELLED": {
        "x": 1766.790997402591,
        "y": 757.8987139146559
      },
      "W_Completeren_aanvraag": {
        "x": 1426.5,
        "y": 483
      },
      "event_A_CANCELLED_W_Completeren_aanvraag_W_Completeren_aanvraag_e16": {
        "x": 1672.7117849332594,
        "y": 626.0510141542533
      },
      "event_W_Afhandelen_leads_W_Afhandelen_leads_W_Afhandelen_leads_e4": {
        "x": 655.761744966443,
        "y": 217.36241610738256
      },
      "event_A_DECLINED_W_Afhandelen_leads_W_Afhandelen_leads_e14": {
        "x": 882.4786666370233,
        "y": 160.09458688940182
      },
      "event_W_Completeren_aanvraag_A_DECLINED_A_DECLINED_e12": {
        "x": 1437.2303085342924,
        "y": 164.15835955473668
      },
      "event_W_Afhandelen_leads_A_PREACCEPTED_A_PREACCEPTED_e5": {
        "x": 818.7339239331988,
        "y": 529.3038582560323
      },
      "event_W_Afhandelen_leads_A_DECLINED_A_DECLINED_e7": {
        "x": 968.844585808992,
        "y": 242.55877020142157
      },
      "W_Afhandelen_leads": {
        "x": 821.417225950783,
        "y": 376.0357941834452
      },
      "event_W_Completeren_aanvraag_W_Afhandelen_leads_W_Afhandelen_leads_e11": {
        "x": 1092.8934977735653,
        "y": 428.26527569570914
      },
      "event_A_PARTLYSUBMITTED_A_PREACCEPTED_A_PREACCEPTED_e2": {
        "x": 593.0861297539151,
        "y": 692.4586129753915
      },
      "A_PARTLYSUBMITTED": {
        "x": 378.5,
        "y": 379
      },
      "event_A_SUBMITTED_A_PARTLYSUBMITTED_A_PARTLYSUBMITTED_e8": {
        "x": 162.44519015659955,
        "y": 404.8076062639821
      },
      "A_PREACCEPTED": {
        "x": 1005.8066255058529,
        "y": 757.3592776813681
      },
      "A_DECLINED": {
        "x": 1067.0906040268455,
        "y": 111.95078299776287
      },
      "event_W_Completeren_aanvraag_A_CANCELLED_A_CANCELLED_e10": {
        "x": 1520.126207421134,
        "y": 705.6105315398788
      },
      "event_W_Afhandelen_leads_W_Completeren_aanvraag_W_Completeren_aanvraag_e6": {
        "x": 1105.8665617810532,
        "y": 523.111331910971
      },
      "event_A_DECLINED_W_Completeren_aanvraag_W_Completeren_aanvraag_e13": {
        "x": 1260.1804658627125,
        "y": 270.0213059368836
      }
    },
    "edges": {
      "s_to_a_A_PARTLYSUBMITTED_event_A_PARTLYSUBMITTED_A_DECLINED_A_DECLINED_e1": {
        "distances": [
          -119.17999227832803
        ],
        "weights": [
          0.5498085448561726
        ]
      },
      "s_to_a_W_Completeren_aanvraag_event_W_Completeren_aanvraag_W_Afhandelen_leads_W_Afhandelen_leads_e11": {
        "distances": [
          15.403787004217037
        ],
        "weights": [
          0.40925822952204444
        ]
      },
      "a_to_s_event_A_DECLINED_W_Completeren_aanvraag_W_Completeren_aanvraag_e13_W_Completeren_aanvraag": {
        "distances": [
          1.3232272336339153
        ],
        "weights": [
          0.4304054009582524
        ]
      },
      "s_to_a_W_Afhandelen_leads_event_W_Afhandelen_leads_A_DECLINED_A_DECLINED_e7": {
        "distances": [
          3.4947134243720224
        ],
        "weights": [
          0.47668888136552146
        ]
      },
      "a_to_s_event_W_Afhandelen_leads_A_DECLINED_A_DECLINED_e7_A_DECLINED": {
        "distances": [
          -1.7384124534845975
        ],
        "weights": [
          0.5636902782134202
        ]
      },
      "a_to_s_event_W_Completeren_aanvraag_W_Afhandelen_leads_W_Afhandelen_leads_e11_W_Afhandelen_leads": {
        "distances": [
          4.812154075357399
        ],
        "weights": [
          0.4849343451066564
        ]
      },
      "s_to_a_A_DECLINED_event_A_DECLINED_W_Completeren_aanvraag_W_Completeren_aanvraag_e13": {
        "distances": [
          5.404292032213013
        ],
        "weights": [
          0.5178314793691127
        ]
      },
      "a_to_s_event_A_PARTLYSUBMITTED_A_DECLINED_A_DECLINED_e1_A_DECLINED": {
        "distances": [
          -83.2332451507466
        ],
        "weights": [
          0.600150832020147
        ]
      }
    }
  },
  "cyLayout_name_Procrastinator_3000": {
    "nodes": {
      "event_fechar_aba_estudar_fechar_abatauestudar_fechar_abatauestudar": {
        "x": 378.91451842485793,
        "y": 38.548246144525166
      },
      "event_Mesa_PC_abrir_youtube_abrir_youtube": {
        "x": 377.69172282704994,
        "y": 523.4968423726486
      },
      "Mesa": {
        "x": 469.9996438075804,
        "y": 216.0680387787006
      },
      "event_abrir_youtube_estudar_abrir_youtubetauestudar_abrir_youtubetauestudar": {
        "x": 172.10424692593577,
        "y": 408.6394685697678
      },
      "Cama": {
        "x": 926.0999999999999,
        "y": 106.4
      },
      "event_dormir_estudar_dormirtauestudar_dormirtauestudar": {
        "x": 615.3477486910994,
        "y": 90.34136125654443
      },
      "event_PC_Mesa_fechar_aba_fechar_aba": {
        "x": 613.190273950199,
        "y": 212.82501603096583
      },
      "PC": {
        "x": 578.0700472756959,
        "y": 474.3425161921071
      },
      "event_estudar_estudar_estudartauestudar_estudartauestudar": {
        "x": 144.61790575916226,
        "y": 44.310366492146585
      },
      "event_Mesa_Mesa_estudar_estudar": {
        "x": 253.07028485674755,
        "y": 225.77783426150992
      },
      "event_Cama_Mesa_despertar_despertar": {
        "x": 551.2149738219896,
        "y": -19.193821989528793
      },
      "event_Mesa_Cama_dormir_dormir": {
        "x": 742.1438752548194,
        "y": 314.9105156148085
      }
    },
    "edges": {}
  },
  "cyLayout_name_suene": {
    "nodes": {
      "event_uv_vz_uvtauvz_uvtauvz": {
        "x": 130.57848145365378,
        "y": 227.88340925583165
      },
      "w": {
        "x": 583.2044555215092,
        "y": -224.79463632715547
      },
      "event_u_v_uv_uv": {
        "x": 81.46541013998139,
        "y": 54.1622371046042
      },
      "v": {
        "x": 421.14041356836344,
        "y": 83.10397346645324
      },
      "event_uv_vwwu_uvtauvwwu_uvtauvwwu": {
        "x": 211.18722361386207,
        "y": -106.5176490600458
      },
      "z": {
        "x": 662.1099130603158,
        "y": 104.02818706782358
      },
      "event_v_w_vw_vw": {
        "x": 560.970922283897,
        "y": -10.555441583698014
      },
      "u": {
        "x": -15.149319094810945,
        "y": -214.09818749089817
      },
      "event_w_w_ww_ww": {
        "x": 604.6252782368027,
        "y": -343.17305297655383
      },
      "event_u_u_uu_uu": {
        "x": -73.20018058771305,
        "y": -270.76503669127453
      },
      "event_w_u_wu_wu": {
        "x": 284.2615438221287,
        "y": -387.4357609325178
      },
      "event_vw_wu_vwwu_vwwu": {
        "x": 386.7857965596302,
        "y": -180.99218431746195
      },
      "event_v_z_vz_vz": {
        "x": 532.5827630316321,
        "y": 163.69363735236934
      }
    },
    "edges": {}
  },
  "cyLayout_name_complete": {
    "nodes": {
      "event_P3_P32_t_p3_p32_t_p3_p32": {
        "x": -507.2999999999997,
        "y": 641.3
      },
      "P61": {
        "x": 4047.3,
        "y": 795.5
      },
      "event_P62_P7a_t_p62_p7a_t_p62_p7a": {
        "x": 722.7,
        "y": 635.3
      },
      "event_C1_D_c1d1_back1": {
        "x": 3182.7,
        "y": 69.19999999999999
      },
      "event_C2_D_dc21_back3": {
        "x": 1497.192512230593,
        "y": -523.0435203196317
      },
      "P14": {
        "x": 2075.7,
        "y": 574.1
      },
      "event_P7a_P7a1_t_p7a_p7a1_t_p7a_p7a1": {
        "x": 968.7,
        "y": 684.8
      },
      "event_P3_P31_t_p3_p31_t_p3_p31": {
        "x": 4416.3,
        "y": 487.7
      },
      "event_P22_P7_t_p22_p7_t_p22_p7": {
        "x": 4170.3,
        "y": 919.0999999999999
      },
      "P32": {
        "x": -384.2999999999997,
        "y": 715.0999999999999
      },
      "C2": {
        "x": 3923.7,
        "y": 223.70000000000002
      },
      "event_P9a2_P11_t_p9a2_p11_t_p9a2_p11": {
        "x": 5154.299999999999,
        "y": 647.9
      },
      "event_P14_P15_t_p14_p15_t_p14_p15": {
        "x": 2198.7,
        "y": 118.40000000000003
      },
      "event_P10_P11_t_p10_p11_t_p10_p11": {
        "x": 5154.299999999999,
        "y": 887.5999999999999
      },
      "P62": {
        "x": 599.7000000000003,
        "y": 635.3
      },
      "P2": {
        "x": 3800.1,
        "y": 721.7
      },
      "event_C1_D_c1d2_back2": {
        "x": 3182.7,
        "y": 167.60000000000002
      },
      "P4a": {
        "x": 4785.299999999999,
        "y": 487.7
      },
      "P6": {
        "x": 353.7000000000003,
        "y": 715.0999999999999
      },
      "P21": {
        "x": 4047.3,
        "y": 500.3
      },
      "P7": {
        "x": 4293.3,
        "y": 887.5999999999999
      },
      "P9a1": {
        "x": 1829.7,
        "y": 574.1
      },
      "P11": {
        "x": 5277.299999999999,
        "y": 813.8
      },
      "event_D_P1_t_d_p1_t_d_p1": {
        "x": 3429.9,
        "y": 721.7
      },
      "P7a1": {
        "x": 1091.7,
        "y": 684.8
      },
      "P5": {
        "x": 107.70000000000027,
        "y": 715.0999999999999
      },
      "event_P6_P61_t_p6_p61_t_p6_p61": {
        "x": 3923.7,
        "y": 795.5
      },
      "event_P31_P4a_t_p31_p4a_t_p31_p4a": {
        "x": 4662.3,
        "y": 487.7
      },
      "P9a2": {
        "x": 5031.299999999999,
        "y": 647.9
      },
      "event_P2_P22_t_p2_p22_t_p2_p22": {
        "x": 3923.7,
        "y": 919.0999999999999
      },
      "event_back2_bfaux_back2taubfaux_back2taubfaux": {
        "x": 3306.3,
        "y": 359
      },
      "event_P13_P14_t_p13_p14_t_p13_p14": {
        "x": 5892.299999999999,
        "y": 776.9
      },
      "event_P7a_P7a2_t_p7a_p7a2_t_p7a_p7a2": {
        "x": 968.7,
        "y": 537.2
      },
      "event_F_C1_dfc1_enterC1": {
        "x": 2936.7,
        "y": 118.40000000000003
      },
      "event_back4_dfaux_back4taudfaux_back4taudfaux": {
        "x": 4170.3,
        "y": 209.9
      },
      "event_P8_P9_t_p8_p9_t_p8_p9": {
        "x": 4662.3,
        "y": 887.5999999999999
      },
      "P1": {
        "x": 2696.2885863498436,
        "y": 370.6982500165213
      },
      "event_P1_P2_t_p1_p2_t_p1_p2": {
        "x": 3676.5,
        "y": 721.7
      },
      "event_P9a_P9a1_t_p9a_p9a1_t_p9a_p9a1": {
        "x": 1706.7,
        "y": 574.1
      },
      "event_P4a_P5_t_p4a_p5_t_p4a_p5": {
        "x": 4908.299999999999,
        "y": 180.20000000000002
      },
      "P31": {
        "x": 4539.3,
        "y": 487.7
      },
      "P10": {
        "x": 5031.299999999999,
        "y": 887.5999999999999
      },
      "event_P15_P16_t_p15_p16_t_p15_p16": {
        "x": 2444.7,
        "y": 118.40000000000003
      },
      "P7a": {
        "x": 845.7,
        "y": 635.3
      },
      "P9": {
        "x": 4785.299999999999,
        "y": 887.5999999999999
      },
      "C1": {
        "x": 3059.7,
        "y": 118.40000000000003
      },
      "event_P9_P10_t_p9_p10_t_p9_p10": {
        "x": 4908.299999999999,
        "y": 887.5999999999999
      },
      "event_P2_P21_t_p2_p21_t_p2_p21": {
        "x": 3923.7,
        "y": 500.3
      },
      "event_P9a1_P14_t_p9a1_p14_t_p9a1_p14": {
        "x": 1952.7,
        "y": 574.1
      },
      "P7a2": {
        "x": 1091.7,
        "y": 537.2
      },
      "event_P4_P5_t_p4_p5_t_p4_p5": {
        "x": -15.299999999999727,
        "y": 715.0999999999999
      },
      "P8": {
        "x": 4539.3,
        "y": 887.5999999999999
      },
      "event_P9a_P9a2_t_p9a_p9a2_t_p9a_p9a2": {
        "x": 4908.299999999999,
        "y": 647.9
      },
      "F": {
        "x": 2813.7,
        "y": 118.40000000000003
      },
      "P15": {
        "x": 2321.7,
        "y": 118.40000000000003
      },
      "event_P32_P4_t_p32_p4_t_p32_p4": {
        "x": -261.2999999999997,
        "y": 715.0999999999999
      },
      "event_back1_bf_back1taubf_back1taubf": {
        "x": 3306.3,
        "y": 62.900000000000034
      },
      "event_P7a1_P8a_t_p7a1_p8a_t_p7a1_p8a": {
        "x": 1214.7,
        "y": 684.8
      },
      "event_P12_P13_t_p12_p13_t_p12_p13": {
        "x": 5646.299999999999,
        "y": 813.8
      },
      "event_P61_P7_t_p61_p7_t_p61_p7": {
        "x": 4170.3,
        "y": 795.5
      },
      "P4": {
        "x": -138.29999999999973,
        "y": 715.0999999999999
      },
      "D": {
        "x": 769.3530396043999,
        "y": -25.92370564711331
      },
      "event_P5_P6_t_p5_p6_t_p5_p6": {
        "x": 230.70000000000027,
        "y": 715.0999999999999
      },
      "event_C2_D_dc22_back4": {
        "x": 4047.3,
        "y": 309.5
      },
      "Faux": {
        "x": 3676.5,
        "y": 223.70000000000002
      },
      "P12": {
        "x": 5523.299999999999,
        "y": 813.8
      },
      "event_P21_P3_t_p21_p3_t_p21_p3": {
        "x": 4170.3,
        "y": 500.3
      },
      "event_P11_P12_t_p11_p12_t_p11_p12": {
        "x": 5400.299999999999,
        "y": 813.8
      },
      "event_P6_P62_t_p6_p62_t_p6_p62": {
        "x": 476.7000000000003,
        "y": 635.3
      },
      "event_back2_dfaux_bfaux_bfaux": {
        "x": 3429.9,
        "y": 210.20000000000002
      },
      "event_back2_bf_back2taubf_back2taubf": {
        "x": 3306.3,
        "y": -35.5
      },
      "P9a": {
        "x": 1583.7,
        "y": 635.3
      },
      "event_P7a2_P8aa_t_p7a2_p8aa_t_p7a2_p8aa": {
        "x": 1214.7,
        "y": 537.2
      },
      "event_P16_F_t_p16_a_t_p16_a": {
        "x": 2690.7,
        "y": 118.40000000000003
      },
      "event_back2_df_bf_bf": {
        "x": 3429.9,
        "y": 13.699999999999989
      },
      "event_Faux_C2_c2fx_enterC2": {
        "x": 3800.1,
        "y": 223.70000000000002
      },
      "P8aa": {
        "x": 1337.7,
        "y": 537.2
      },
      "event_P8a_P9a_t_p8a_p9a_t_p8a_p9a": {
        "x": 1460.7,
        "y": 684.8
      },
      "event_D_Faux_dfaux_dfaux": {
        "x": 3552.9,
        "y": 161
      },
      "P22": {
        "x": 4047.3,
        "y": 919.0999999999999
      },
      "event_P7_P8_t_p7_p8_t_p7_p8": {
        "x": 4416.3,
        "y": 887.5999999999999
      },
      "P16": {
        "x": 2567.7,
        "y": 118.40000000000003
      },
      "event_back4_df_back4taudf_back4taudf": {
        "x": -576.651981731438,
        "y": 829.5423399702723
      },
      "P13": {
        "x": 5769.299999999999,
        "y": 813.8
      },
      "P8a": {
        "x": 1337.7,
        "y": 684.8
      },
      "event_P8aa_P9a_t_p8aa_p9a_t_p8aa_p9a": {
        "x": 1460.7,
        "y": 537.2
      },
      "P3": {
        "x": 4293.3,
        "y": 487.7
      }
    },
    "edges": {}
  }
}