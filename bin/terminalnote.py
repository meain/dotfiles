# This is very basic terminal note taking app.
'''

Intentd to be used with some alias in you .*rc
Notes are saved as .terminalnotesdata in root directory
? second, third, fourth arguments can be used as tag
Completed/deleted entries will be stored with a # so that it can be recognized later
All notes are sperated by a '\n---\n'
How to use it:
    * To display all non-complete entries
        $ k
    * To display all entries
        $ k a
    * To create a new entry
        $ k "new entry"
    * To delete an entry - first get the note number by disply - and delete the note on entering the number
        $ k 5

'''

import argparse
import sys

class bcolors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    ORANGE = '\033[93m'
    RED = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

notes_filename = '/Users/abinsimon/.terminalnotesdata'

def is_number(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

def extract_note(number):
    notesfile = open(notes_filename, 'r+')
    notesfile.seek(0,0)
    count = 0
    note = ""
    available = False
    line_number = 0
    done_notes = 0
    done_note_seen_flag = False
    # get files into a list
    # content = [line.strip() for line in notesfile]
    with notesfile as f:
        content = f.readlines()
    # print content
    # If no notes exist
    if len(content) == 0:
        return False, "", 0
    while True:
        count += 1
        done_note_seen_flag = False
        if content[line_number] == '--\n' and content[line_number + 1] == '---\n' and content[line_number + 2] == '\n':
            margin = 1
            while content[line_number + 2 + margin] != '--\n':
                # Break if done
                if content[line_number + 2 + margin][0] == '#':
                    if done_note_seen_flag == False:
                        done_notes += 1
                    done_note_seen_flag = True
                available = True
                note += content[line_number + 2 + margin]
                margin += 1
                if ( line_number + 2 + margin ) >= len(content):
                    break
            line_number = line_number + 2 + margin
            # print content[line_number]
        if count - done_notes != number:
            available = False
            note = ""
        if count - done_notes == number or ( line_number ) >= len(content):
            break
    actual_count = count - done_notes
    return available, note, actual_count

def write_new_entry(data):
    notesfile = open(notes_filename, 'r+')
    if data != "":
        notesfile.seek(0, 2)
        if data[1] == '\n':
            note = "--\n---\n\n" + data[2:] + "\n"
        else:
            note = "--\n---\n\n" + data[1:] + "\n"
        notesfile.write(note)
    notesfile.close()

def display_notes():
    count = 1
    while True:
        new_line_count = 0
        available, note, actual_count = extract_note(count)
        if available:
            sys.stdout.write(bcolors.BLUE + "Note "  + str(actual_count) + " : " + bcolors.ENDC)
            for i in range(len(note)):
                if note[i] == '\n':
                    new_line_count += 1
            if new_line_count > 1:
                print ""
            print note
            count += 1
        else:
            break

def delete_note(number):
    notesfile = open(notes_filename, 'r')
    notesfile.seek(0,0)
    count = 0
    note = ""
    deleted = False
    line_number = 0
    done_notes = 0
    done_note_seen_flag = False
    # get files into a list
    # content = [line.strip() for line in notesfile]
    with notesfile as f:
        content = f.readlines()
    # print content
    # If no notes exist
    if len(content) == 0:
        return False, "", 0
    while True:
        count += 1
        done_note_seen_flag = False
        if content[line_number] == '--\n' and content[line_number + 1] == '---\n' and content[line_number + 2] == '\n':
            margin = 1
            while content[line_number + 2 + margin] != '--\n':
                # Break if done
                if content[line_number + 2 + margin][0] == '#':
                    if done_note_seen_flag == False:
                        done_notes += 1
                    done_note_seen_flag = True
                if count - done_notes == number:
                    deleted = True
                    sys.stdout.write(bcolors.ORANGE + content[line_number + 2 + margin])
                    content[line_number + 2 + margin] = "#" + content[line_number + 2 + margin]
                    margin += 1
                else:
                    margin += 1
                if ( line_number + 2 + margin ) >= len(content):
                    break
            line_number = line_number + 2 + margin
        # notesfile = open('.terminalnotesdata', 'r')
        # with notesfile as f:
        #     content = f.readlines()
        if count - done_notes != number:
            deleted = False
            note = ""
        if count - done_notes == number or ( line_number ) >= len(content):
            break
    if deleted == True:
        print bcolors.RED + "Note deleted"
        nfile = open(notes_filename, 'w')
        for item in content:
            nfile.write("%s" % item)
        nfile.close
    return deleted, note

def main():
    parser = argparse.ArgumentParser(description = "Terminal note taking app \n \"\" - for note, number - delete specific note, nothing to display all notes")
    parser.add_argument('string', help = 'Control behaviour')
    args = parser.parse_args()
    data = args.string

    if data == 'k':
        # print bcolors.HEADER + "   === NOTES ===" + bcolors.ENDC
        display_notes()
    elif is_number(data):
        # print "Deleted note" + data
        delete_note(int(data))
    else:
        write_new_entry(data)



#Final call and doing the "thing"
if __name__ == '__main__':
    main()

