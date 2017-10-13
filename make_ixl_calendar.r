make_ixl_calendar_two_subject <- function(x_date_start, x_hour_start, xs_subject_name_a, x_vec_section_cnt_a, xs_subject_name_b, x_vec_section_cnt_b, x_section_code_type=11) {

    xi_section_a_remain = sum(x_vec_section_cnt_a);
    xi_section_b_remain = sum(x_vec_section_cnt_b);
    xi_section_tot = xi_section_a_remain + xi_section_b_remain;
    x_date = x_date_start - 1;

    xi_chap_a_seq = 1;
    xi_section_a_seq = 0;
    xi_section_a_cnt = x_vec_section_cnt_a[xi_chap_a_seq];

    xi_chap_b_seq = 1;
    xi_section_b_seq = 0;
    xi_section_b_cnt = x_vec_section_cnt_b[xi_chap_b_seq];

    x_vec_line = c();

    for (xa in 1:xi_section_tot) {
        x_date = x_date + 1;

        xi_date_week_day = as.numeric(x_date - as.Date("2000-01-02")) %% 7;
        xi_date_week_seq_a = as.numeric(x_date_start - as.Date("2000-01-02")) %/% 7;
        xi_date_week_seq_b = as.numeric(x_date - as.Date("2000-01-02")) %/% 7;
        xi_date_week_seq = xi_date_week_seq_b - xi_date_week_seq_a;

        x_subject_code = 0;
        if ((xi_section_a_remain > 0)&&((xi_date_week_seq %% 2) == 0)) {
            xi_section_a_remain = xi_section_a_remain - 1;
            x_subject_code = 1;
        } else if ((xi_section_b_remain > 0)&&((xi_date_week_seq %% 2) == 1)) {
            xi_section_b_remain = xi_section_b_remain - 1;
            x_subject_code = 2;
        } else if (xi_section_a_remain > 0) {
            xi_section_a_remain = xi_section_a_remain - 1;
            x_subject_code = 1;
        } else if (xi_section_b_remain > 0) {
            xi_section_b_remain = xi_section_b_remain - 1;
            x_subject_code = 2;
        }

        xs_line = "";

        if (x_subject_code == 1) {
            xi_section_a_seq = xi_section_a_seq + 1;

            if (x_section_code_type >= 20) {
                x_char = "A";
                if (xi_section_a_seq <= 26) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("A"))+xi_section_a_seq-1));
                } else if (xi_section_a_seq <= 52) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("AA"))+xi_section_a_seq-26-1));
                }

                xs_line = paste(xs_subject_name_a, " ", xi_chap_a_seq, x_char, ", ", x_date, ", ", x_date, ", ", x_hour_start, ":00, ", (x_hour_start+1), ":00", sep="");
            } else {
                x_char = "A";
                if (xi_chap_a_seq <= 26) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("A"))+xi_chap_a_seq-1));
                } else if (xi_chap_a_seq <= 52) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("AA"))+xi_chap_a_seq-26-1));
                }

                xs_line = paste(xs_subject_name_a, " ", x_char, xi_section_a_seq, ", ", x_date, ", ", x_date, ", ", x_hour_start, ":00, ", (x_hour_start+1), ":00", sep="");
            }

            if (xi_section_a_seq == xi_section_a_cnt) {
                xi_chap_a_seq = xi_chap_a_seq + 1;
                xi_section_a_seq = 0;
                xi_section_a_cnt = x_vec_section_cnt_a[xi_chap_a_seq];
            }
        } else if (x_subject_code == 2) {
            xi_section_b_seq = xi_section_b_seq + 1;

            if ((x_section_code_type %% 10) >= 2) {
                x_char = "A";
                if (xi_section_b_seq <= 26) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("A"))+xi_section_b_seq-1));
                } else if (xi_section_b_seq <= 52) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("AA"))+xi_section_b_seq-26-1));
                }

                xs_line = paste(xs_subject_name_b, " ", xi_chap_b_seq, x_char, ", ", x_date, ", ", x_date, ", ", x_hour_start, ":00, ", (x_hour_start+1), ":00", sep="");
            } else {
                x_char = "A";
                if (xi_chap_b_seq <= 26) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("A"))+xi_chap_b_seq-1));
                } else if (xi_chap_b_seq <= 52) {
                    x_char = rawToChar(as.raw(as.numeric(charToRaw("AA"))+xi_chap_b_seq-26-1));
                }

                xs_line = paste(xs_subject_name_b, " ", x_char, xi_section_b_seq, ", ", x_date, ", ", x_date, ", ", x_hour_start, ":00, ", (x_hour_start+1), ":00", sep="");
            }

            if (xi_section_b_seq == xi_section_b_cnt) {
                xi_chap_b_seq = xi_chap_b_seq + 1;
                xi_section_b_seq = 0;
                xi_section_b_cnt = x_vec_section_cnt_b[xi_chap_b_seq];
            }
        }

        x_vec_line[xa] = xs_line;

    }

    return(x_vec_line);
}

make_ixl_calendar_csv <- function(x_vec_ixl_calendar, x_file_out_name) {

    x_file_created=0;
    write(x_vec_ixl_calendar, file=x_file_out_name, sep="\n", append=FALSE);
    x_file_created = x_file_created + 1;

    return(x_file_created);
}

main <- function() {
    x_vec_geometry = c(10, 9, 8, 7, 8, 4, 6, 5, 8, 3, 11, 16, 9, 12, 4, 15, 4, 13, 12, 9, 17, 5, 8, 7, 7);
    x_vec_language_10 = c(10, 4, 9, 13, 5, 15, 10, 8, 3, 7, 3, 9, 8, 8, 7, 6, 7);
    x_vec_social_8 = c(5, 6, 1, 3, 4, 8, 2, 7, 2, 1, 2, 2, 2, 13, 13, 15, 2);
    x_vec_science_8 = c(1, 5, 1, 5, 1, 1, 1, 3, 5, 2, 6, 6, 3, 1, 3, 1, 3, 1, 1, 2, 3, 6);
    x_vec_spanish = c(18, 16, 20, 18, 22, 20, 21);

    x_vec_ixl_calendar = c("Subject, Start Date, End Date, Start Time, End Time");

    xs_line = make_ixl_calendar_two_subject(as.Date("2017-10-10"), 17, "Geometry", x_vec_geometry, "", c(), x_section_code_type=11);
    x_vec_ixl_calendar = c(x_vec_ixl_calendar, xs_line);

    xs_line = make_ixl_calendar_two_subject(as.Date("2017-10-10"), 18, "Language 10th Grade", x_vec_language_10, "Social 8th Grade", x_vec_social_8, x_section_code_type=11);
    x_vec_ixl_calendar = c(x_vec_ixl_calendar, xs_line);

    xs_line = make_ixl_calendar_two_subject(as.Date("2017-10-15"), 19, "Science 8th Grade", x_vec_science_8, "Spanish", x_vec_spanish, x_section_code_type=12);
    x_vec_ixl_calendar = c(x_vec_ixl_calendar, xs_line);

    x_file_path = Sys.getenv("USERPROFILE");
    x_file_out_name = paste(x_file_path, "\\Documents\\user_home\\github\\ychenrepo\\ixl_calendar.csv", sep="");

    x_file_created = make_ixl_calendar_csv(x_vec_ixl_calendar, x_file_out_name);

    if (x_file_created > 0) {
        print(x_file_out_name);
    }

}

main();
